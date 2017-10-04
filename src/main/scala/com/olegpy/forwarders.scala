package com.olegpy

import scala.meta._
import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq

class forwarders extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    import forwarders._

    defn match {
      case MaybeCompanion(TraitOrClass(orig, name, tps, stats), companionO) =>
        val companion = companionO getOrElse {
          q"""object ${Term.Name(name.value)}"""
        }

        q"""
           $orig
           ${expand(name, tps, stats, companion)}
         """
      case _ =>
        abort("@forwarders can only be used on a trait")
    }
  }
}

object forwarders {
  object MaybeCompanion {
    def unapply(a: Any): Option[(Defn, Option[Defn.Object])] = a match {
      case Term.Block(Seq(cls: Defn, companion: Defn.Object)) => Some((cls, Some(companion)))
      case cls: Defn => Some((cls, None))
      case _ => None
    }
  }

  object TraitOrClass {
    def unapply(defn: Defn): Option[(Defn, Type.Name, Seq[Type.Param], Seq[Stat])] =
      defn match {
        case cls @ Defn.Class(_, name, tps, _, tmpl) => Some((cls, name, tps, tmpl.stats.to[Seq].flatten))
        case trt @ Defn.Trait(_, name, tps, _, tmpl) => Some((trt, name, tps, tmpl.stats.to[Seq].flatten))
        case _ => None
      }
  }

  object PublicOnly {
    def unapply(mods: Seq[Mod]): Boolean = !mods.exists {
      case Mod.Private(_) | Mod.Protected(_) => true
      case _ => false
    }
  }

  def expand(
    typeName: Type.Name,
    tParams: Seq[Type.Param],
    stats: Seq[Stat],
    companion: Defn.Object
  ): Defn.Object = {
    val evName = Term.fresh(typeName.value)

    val traitTypeParamNames = tParams.map(_.name.value).toSet

    def addTypes(tparams: Seq[Type.Param]) =
      if (!tparams.exists(tp => traitTypeParamNames(tp.name.value))) tParams ++ tparams
      else abort("Shadowing of type parameters is not supported")

    def addImplicit(paramss: Seq[Seq[Term.Param]]) = {
      val tt = tParams.map(p => Type.Name(p.name.value))
      val evType = t"$typeName[..$tt]"
      val p = param"implicit $evName: $evType"

      val updatedLast = for {
        lastList   <- paramss.lastOption
        firstParam <- lastList.headOption
        if firstParam.mods.exists(_.is[Mod.Implicit])
      } yield paramss.init :+ (p +: lastList)

      updatedLast.getOrElse(paramss :+ Seq(p))
    }

    def writeDef(
      name: Term.Name,
      tparams: Seq[Type.Param],
      paramss: Seq[Seq[Term.Param]],
      decltpe: Option[Type]
    ) = {
      val args = paramss.map(_.map(p => Term.Name(p.name.value)))
      decltpe.map(tpe =>
        q"def $name[..${addTypes(tparams)}](...${addImplicit(paramss)}): $tpe = $evName.$name(...$args)"
      ).getOrElse(
        q"def $name[..${addTypes(tparams)}](...${addImplicit(paramss)}) = $evName.$name(...$args)"
      )
    }

    val mkDelegates: Stat => Seq[Stat] = {
      case Decl.Val(PublicOnly(), pats, decl) =>
        pats.map(term => writeDef(term.name, Seq(), Seq(), Some(decl)))

      case Defn.Val(PublicOnly(), pats, decl, _) =>
        pats.collect {
          case Pat.Var.Term(name) => writeDef(name, Seq(), Seq(), decl)
        }

      case Decl.Def(PublicOnly(), name, tparams, paramss, decltpe) =>
        Seq(writeDef(name, tparams, paramss, Some(decltpe)))

      case Defn.Def(PublicOnly(), name, tparams, paramss, decltpe, _) =>
        Seq(writeDef(name, tparams, paramss, decltpe))

      case _ => Seq()
    }

    val hasApply = stats.exists {
      case Decl.Val(_, pats, _)    => pats.exists(_.name == "apply")
      case Defn.Val(_, pats, _, _) => pats.exists {
        case Pat.Var.Term(Term.Name("apply")) => true
        case _ => false
      }
      case Decl.Def(_, Term.Name("apply"), _, _, _) => true
      case Defn.Def(_, Term.Name("apply"), _, _, _, _) => true
      case _ => false
    }

    val typeclassApply =
      if (hasApply || tParams.isEmpty) Seq()
      else Seq(q"def apply[..$tParams](...${addImplicit(Seq())}) = $evName")

    val opsObject = q""" object ops { ..${stats flatMap mkDelegates} }"""

    val newStats = Seq(
      typeclassApply,
      Seq(opsObject),
      companion.templ.stats.to[Seq].flatten
    ).flatten

    companion.copy(templ = companion.templ.copy(stats = Some(newStats)))
  }
}

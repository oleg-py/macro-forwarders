package com.olegpy

import scala.meta._
import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq

class forwarders extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case orig @ q"trait $name[..$tps] { ..$stats }" =>
        q"""
           $orig
           ${forwarders.expand(name, tps, stats)}
         """
      case orig @ q"trait $name { ..$stats }" =>
        q"""
           $orig
           ${forwarders.expand(name, Seq(), stats)}
         """
      case _ =>
        abort("@algebra can only be used on a trait")
    }
  }
}

object forwarders {
  def expand(traitName: Type.Name, traitTParams: Seq[Type.Param], traitStats: Seq[Stat]): Stat = {
    val evName = Term.fresh(traitName.value)

    val traitTypeParamNames = traitTParams.map(_.name.value).toSet

    def addTypes(tparams: Seq[Type.Param]) =
      if (!tparams.exists(tp => traitTypeParamNames(tp.name.value))) traitTParams ++ tparams
      else abort("Shadowing of type parameters is not supported")

    def addImplicit(paramss: Seq[Seq[Term.Param]]) = {
      val tt = traitTParams.map(p => Type.Name(p.name.value))
      val evType = t"$traitName[..$tt]"
      val p = param"implicit $evName: $evType"

      val updatedLast = for {
        lastList   <- paramss.lastOption
        firstParam <- lastList.headOption
        if firstParam.mods.exists(_.is[Mod.Implicit])
      } yield paramss.init :+ (lastList :+ p)

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

    val enhance: Stat => Seq[Stat] = {
      case Decl.Val(_, pats, decl) =>
        pats.map(term => writeDef(term.name, Seq(), Seq(), Some(decl)))

      case Defn.Val(_, pats, decl, _) =>
        pats.collect {
          case Pat.Var.Term(name) => writeDef(name, Seq(), Seq(), decl)
        }

      case Decl.Def(_, name, tparams, paramss, decltpe) =>
        Seq(writeDef(name, tparams, paramss, Some(decltpe)))

      case Defn.Def(_, name, tparams, paramss, decltpe, _) =>
        Seq(writeDef(name, tparams, paramss, decltpe))

      case _ => Seq()
    }

    val hasApply = traitStats.exists {
      case Decl.Val(_, pats, _)    => pats.exists(_.name == "apply")
      case Defn.Val(_, pats, _, _) => pats.exists {
        case Pat.Var.Term(Term.Name("apply")) => true
        case _ => false
      }
      case Decl.Def(_, Term.Name("apply"), _, _, _) => true
      case Defn.Def(_, Term.Name("apply"), _, _, _, _) => true
      case _ => false
    }

    val typeclassApply = if (hasApply || traitTParams.isEmpty) Seq()
                         else Seq(q"def apply[..$traitTParams](...${addImplicit(Seq())}) = $evName")


    q"""
      object ${Term.Name(traitName.value)} {
        ..$typeclassApply
        ..${traitStats flatMap enhance}
      }
    """
  }
}

import scala.util.parsing.combinator.RegexParsers

object Main extends App with RegexParsers {

      val model =
        """
          |btsmodel {
          |
          | typedef {
          |   String -> java.lang.String
          |   Long -> java.lang.Long
          | }
          |
          | module RBP {
          |
          |   entities {
          |
          |     entity Foo {
          |      id : Long
          |      x : String
          |      y : String
          |     }
          |
          |    entity Bar {
          |      a : String
          |      b : String
          |    }
          |
          |   }
          | }
          |
          | module TM {
          |
          |      entities {
          |           entity XYZ {
          |
          |           }
          |      }
          |
          |      tasks {
          |           task ABC {
          |
          |           }
          |      }
          | }
          |
          |}
        """.stripMargin


      def attributeTypeParser = "[a-zA-Z]+".r
      def attributeNameParser = "[a-zA-Z]+".r
      def attributeParser : Parser[Attribute] = (attributeNameParser <~ ":") ~ attributeTypeParser ^^ {
        case attributeName ~ fullQualifiedTypeName => Attribute(attributeName, fullQualifiedTypeName)
      }


      def entityBodyParser : Parser[List[Any]]= rep(attributeParser)
      def entityNameParser = "[a-zA-Z]+".r

      def entityParser =  "entity" ~> entityNameParser ~ ("{" ~>entityBodyParser <~ "}") ^^ {
        case entityName ~ entityBody => {
          Entity(entityName, entityBody.filter(_.isInstanceOf[Attribute]).map(_.asInstanceOf[Attribute]))
        }
      }

      def moduleNameParser = "[a-zA-Z]+".r
      def moduleParser = "module" ~> moduleNameParser ~ ("{" ~> moduleBodyParser <~ "}") ^^ {
        case moduleName ~ moduleBody => Module(moduleName, moduleBody)
      }

      def moduleBodyParser : Parser[List[Entity]] = opt("entities" ~ "{" ~> rep(entityParser) <~ "}") ^^ {
        _.getOrElse(Nil)
      }

      def modelParser : Parser[Model] = "btsmodel" ~ "{" ~> typedefParser ~ rep(moduleParser) <~ "}" ^^ {
        case typedefs ~ modules => Model(typedefs, modules)
      }

      def typeDefNameParser = "[0-9a-zA-Z]+".r
      def typeDefFullQualifiedNameParser = "[0-9a-zA-Z.]+".r
      def typedefParser = opt(("typedef" ~ "{") ~> rep(typedefBodyParser) <~ "}") ^^ {
        _.getOrElse(Nil)
      }

      def typedefBodyParser : Parser[TypeDef] = typeDefNameParser ~ ("->" ~> typeDefFullQualifiedNameParser) ^^ {
        case typeDefName ~ typeDefFullQualifiedName => TypeDef(typeDefName,typeDefFullQualifiedName)
      }

      parseAll(modelParser, model) match {
        case Success(model, _) => println(ModelRenderer.render(model))
        case x => println("not success: " + x)
      }

}

case class Model(typedefs : List[TypeDef], modules : List[Module])
case class Entity(name : String, attributes : List[Attribute])
case class Attribute(name : String, _type : String)
case class TypeDef(name : String, fullQualifiedName : String)
case class Module(moduleName : String, entities : List[Entity])
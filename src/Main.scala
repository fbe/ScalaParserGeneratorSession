import scala.util.parsing.combinator.RegexParsers

object Main extends App with RegexParsers {

      val model =
        """
          |model {
          |
          | entity Foo {
          |   id : java.lang.Long
          |   x : String
          |   y : String
          | }
          |
          | entity Bar {
          |   a : String
          |   b : String
          | }
          |
          |}
        """.stripMargin

      def entityBodyParser : Parser[List[Any]]= rep(attributeParser)

      def attributeParser : Parser[Attribute] = (attributeNameParser <~ ":") ~ attributeFullQualifiedTypeNameParser ^^ {
        case attributeName ~ fullQualifiedTypeName => Attribute(attributeName, fullQualifiedTypeName)
      }

      def attributeFullQualifiedTypeNameParser = "[a-zA-Z.]+".r
      def attributeNameParser = "[a-zA-Z]+".r
      def entityNameParser = "[a-zA-Z]+".r

      def entityParser =  "entity" ~> entityNameParser ~ ("{" ~>entityBodyParser <~ "}") ^^ {
        case entityName ~ entityBody => {
          Entity(entityName, entityBody.filter(_.isInstanceOf[Attribute]).map(_.asInstanceOf[Attribute]))
        }
      }

      def modelParser : Parser[Model] = "model" ~ "{" ~> rep(entityParser) <~ "}" ^^ {
        case entities => Model(entities)
      }

      parseAll(modelParser, model) match {
        case Success(model, _) => println(ModelRenderer.render(model))
        case x => println("not success: " + x)
      }

}

case class Model(entities : List[Entity])
case class Entity(name : String, attributes : List[Attribute])
case class Attribute(name : String, fullQualifiedType : String)

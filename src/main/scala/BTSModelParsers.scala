import scala.util.parsing.combinator.RegexParsers

/**
 * Ok, what do we want to parse here?
 *
 *
btsmodel {

 typedef {
   String -> java.lang.String
   Long -> java.lang.Long
 }

 module RBP {

   entities {

     entity Foo {
      id : Long
      x : String
      y : String
     }

    entity Bar {
      a : String
      b : String
    }

   }
 }

 module TM {

      entities {
           entity XYZ {

           }

           entity CallRBPTask {

           }
      }

      tasks {

           task ABC {
           }
      }
 }

}
 */

trait BTSModelParsers extends RegexParsers {

  def attributeTypeParser = "[a-zA-Z]+".r
  def attributeNameParser = "[a-zA-Z]+".r
  def entityNameParser = "[a-zA-Z]+".r
  def taskNameParser = "[a-zA-Z]+".r
  def typeDefNameParser = "[0-9a-zA-Z]+".r
  def typeDefFullQualifiedNameParser = "[0-9a-zA-Z.]+".r
  def moduleNameParser = "[a-zA-Z]+".r

  def attributeParser : Parser[Attribute] = (attributeNameParser <~ ":") ~ attributeTypeParser ^^ {
    case attributeName ~ fullQualifiedTypeName => Attribute(attributeName, fullQualifiedTypeName)
  }


  def modelParser : Parser[Model] = blockParser("btsmodel", "", rep(typeDefContextParser | moduleContextParser)){
    (_, _, typeDefOrModuleContextList) => Model(typeDefOrModuleContextList.filter(_.isInstanceOf[TypeDefContext]).map(_.asInstanceOf[TypeDefContext]), typeDefOrModuleContextList.filter(_.isInstanceOf[ModuleContext]).map(_.asInstanceOf[ModuleContext]))
  }

  case class Block[T](blockMarker : String, blockName : String, blockBody : T)  // TODO invariant?

  /**
   * Parse a block like this
   *
   * blockMarker blockName {
   *  blockBody
   * }
   *
   * or this if you leave the nameParser as ""
   *
   * blockMarker {
   *  blockBody
   * }
   *
   * @param marker parser of the block marker (string)
   * @param name parser of the block name (string)
   * @param body parser of the block body (Parser[B])
   * @param resultTransformer method to be applied to the parsing result (marker, name, body)
   * @tparam T the return generic type of the Parser[T]
   * @tparam B the type of the body to be parsed
   * @return a parser for parsing the given block
   */
  def blockParser[T, B](marker : Parser[String], name : Parser[String], body : Parser[B])(resultTransformer : (String, String, B) => T) : Parser[T] = {
    marker ~ name ~ ("{" ~> body <~ "}") ^^ {
      case marker ~ name ~ body => resultTransformer(marker, name, body)
    }
  }

  def typeDefContextParser : Parser[TypeDefContext] = blockParser("typedef", "", rep(typeDefParser)){
    (blockMarker, blockName, blockBody) => TypeDefContext(blockBody)
  }

  def typeDefParser : Parser[TypeDef] = typeDefNameParser ~ "->" ~ typeDefFullQualifiedNameParser ^^ {
    case typeDefName ~ _ ~ fullQualifiedName => TypeDef(typeDefName, fullQualifiedName)
  }

  def moduleContextParser : Parser[ModuleContext] = blockParser("module", moduleNameParser , rep(entityContextParser | taskContextParser)){
    case (blockMarker, blockName, entityOrTaskContexts) => {
      val entityContexts = entityOrTaskContexts.filter(_.isInstanceOf[EntityContext]).asInstanceOf[List[EntityContext]]
      val taskContexts =  entityOrTaskContexts.filter(_.isInstanceOf[TaskContext]).asInstanceOf[List[TaskContext]]

      entityOrTaskContexts.partition(_.isInstanceOf[EntityContext])._1.asInstanceOf[List[EntityContext]]

      var entities : List[Entity] = Nil
      var tasks : List[Task] = Nil


      entityContexts.foldLeft(List[Entity]())((entityList, ne) => entityList ++ ne.entities)

      entityContexts.map(entities ++ _.entities)
      taskContexts.map(tasks ++ _.tasks)

      ModuleContext(blockName, EntityContext(entities), TaskContext(tasks))
    }
  }

  def entityContextParser : Parser[EntityContext] = blockParser("entities", "", rep(entityParser))((_,_, body) => EntityContext(body))



  def entityParser : Parser[Entity] = blockParser("entity",  entityNameParser, rep(attributeParser)){
    (_,entityName, entityBody)  => Entity(entityName, entityBody)
  }

  def taskContextParser = blockParser("tasks", "", rep(taskParser)) {
    (_,_, tasks) => TaskContext
  }

  def taskParser : Parser[Task] = blockParser("task", taskNameParser, rep(attributeParser)) {
    (_, taskName, taskBody) => Task(taskName, taskBody)
  }


}

case class Model(typedefContexts : List[TypeDefContext], modules : List[ModuleContext])

case class TypeDefContext(typedefs : List[TypeDef])
case class TypeDef(name : String, fullQualifiedName : String)

case class ModuleContext(moduleName : String, entityContext : EntityContext, taskContext : TaskContext)

case class TaskContext(tasks : List[Task])
case class Task(name : String, attributes : List[Attribute])

case class EntityContext(entities : List[Entity])
case class Entity(name : String, attributes : List[Attribute])
case class Attribute(name : String, _type : String)


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


  def modelParser : Parser[Model] = blockParser("btsmodel"){
    rep(typeDefContextParser | moduleContextParser)
  } ^^ {
    case x : Block[List[Any]] => Model(x.blockBody.filter(_.isInstanceOf[TypeDefContext]).map(_.asInstanceOf[TypeDefContext]), x.blockBody.filter(_.isInstanceOf[ModuleContext]).map(_.asInstanceOf[ModuleContext]))
  }


  case class Block[+T](blockMarker : String, blockName : String, blockBody : T)  // TODO invariant?

  def blockParser[T](blockMarkerParser : Parser[String], blockNameParser : Parser[String] = "")(blockBodyParser : Parser[T]) : Parser[Block[T]] = blockMarkerParser ~ blockNameParser ~ ("{" ~> blockBodyParser <~ "}") ^^ {
    case blockMarker ~ blockName ~ blockBody => Block(blockMarker, blockName, blockBody)
  }

  def typeDefContextParser : Parser[TypeDefContext] = blockParser("typedef"){
    rep(typeDefParser)
  } ^^ {
    case x : Block[List[TypeDef]] => TypeDefContext(x.blockBody)
  }

  def typeDefParser : Parser[TypeDef] = typeDefNameParser ~ "->" ~ typeDefFullQualifiedNameParser ^^ {
    case typeDefName ~ _ ~ fullQualifiedName => TypeDef(typeDefName, fullQualifiedName)
  }

  def moduleContextParser : Parser[ModuleContext] = blockParser("module", moduleNameParser){
    rep(entityContextParser | taskContextParser)
  } ^^ {
    case entityOrTaskContexts : Block[List[Product with Serializable]] => {
      val entityContexts = entityOrTaskContexts.blockBody.filter(_.isInstanceOf[EntityContext]).map(_.asInstanceOf[EntityContext])
      val taskContexts =  entityOrTaskContexts.blockBody.filter(_.isInstanceOf[TaskContext]).map(_.asInstanceOf[TaskContext])

      var entities : List[Entity] = Nil
      var tasks : List[Task] = Nil

      entityContexts.map(entities ++ _.entities)
      taskContexts.map(tasks ++ _.tasks)

      ModuleContext(entityOrTaskContexts.blockName, EntityContext(entities), TaskContext(tasks))
    }
  }

  def entityContextParser : Parser[EntityContext] = blockParser("entities"){
    rep(entityParser)
  } ^^ {
    case entities : Block[List[Entity]] => EntityContext(entities.blockBody)
  }

  def entityParser : Parser[Entity] = blockParser("entity", entityNameParser){
    rep(attributeParser)
  } ^^ {
    case entity : Block[List[Attribute]] => Entity(entity.blockName, entity.blockBody)
  }

  def taskContextParser = blockParser("tasks") {
    rep(taskParser)
  } ^^ {
    case tasks : Block[Task] => TaskContext(tasks.blockBody)
  }

  def taskParser : Parser[Task] = blockParser("task", taskNameParser) {
    rep(attributeParser)
  }  ^^ {
    case taskBody : Block[List[Attribute]] => Task(taskBody.blockName, taskBody.blockBody)
  }

  //case taskContextParser : Parser[TaskContext] =

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


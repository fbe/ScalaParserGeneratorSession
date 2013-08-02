
object Main extends App with BTSModelParsers {

      val modelFiles = List("model.btsmodel", "additionalModel.btsmodel")

      val models = modelFiles.map(modelFile => {

      val model = scala.io.Source.fromFile("src/main/resources/"+modelFile).mkString

      parseAll(modelParser, model) match {
        case Success(model, _) => println(ModelRenderer.render(model))
        case noSuccess => throw new RuntimeException("parser for file " + modelFile + " didn't finish successfully: " + noSuccess);
      }
  });

}


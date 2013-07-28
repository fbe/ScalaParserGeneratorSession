object ModelRenderer {

  val __ = "\t"
  val NL = "\n"

  def render(model: Model): String = {
    model.entities.map(entity => {
      "public class " + entity.name + " {" + NL +
      NL +
        attributeFields(entity) + NL +
      NL +
        attributeAccessor(entity) + NL +
      "}"
    }).mkString("\n")
  }

  def attributeFields(entity : Entity) : String = entity.attributes.map(attribute => {
    __ + "private " + attribute.fullQualifiedType + " " + attribute.name + ";"
  }).mkString("\n")

  def attributeAccessor(entity : Entity) : String = entity.attributes.map(attribute => {
    __ + "public " + attribute.fullQualifiedType + " get" + attribute.name.capitalize + "(){" + NL +
    __ + __ + "return "+attribute.name+";" + NL +
    __ + "}" + NL +
    NL +
    __ + "public void set"+attribute.name.capitalize+"("+attribute.fullQualifiedType+" " + attribute.name + "){" + NL +
    __ + __ + "this."+attribute.name+" = "+attribute.name+";" + NL +
    __ + "}" + NL
  }).mkString("\n")
}

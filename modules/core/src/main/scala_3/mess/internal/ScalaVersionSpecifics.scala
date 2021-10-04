package mess.internal

private[mess] object ScalaVersionSpecifics:
  private[mess] type Factory[-E, +T] = scala.collection.Factory[E, T]

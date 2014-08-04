package miniboxing.tests.compile.bug118


class Vertex[@specialized(Int, Long) +Id]{
  def id: Id = ???
}

class SampleVertexIds {
  def extract(v: Vertex[_]): List[Any] = {
    List(v.id)
  }
}


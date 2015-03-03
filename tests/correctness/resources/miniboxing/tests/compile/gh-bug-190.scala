package miniboxing.tests.compile.bug190

trait Monoid[X] {
  def id: X
}

trait EndofunctionSpace[@miniboxed X] {
  def asMonoidWithCompose = new Monoid[X => X] {
    // crashing since `id` is transformed into MiniboxedFunction1[X,X]
    // and gets a bridge method `id` with signature `X=>X`. The problem
    // is that the @bridge annotation is lost, so the typer complains
    // about a duplicate definition:
    def id = (x: X) => x
  }
}

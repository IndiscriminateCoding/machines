import machines.descriptor.Is

package object machines {
  type Process[+F[_], -I, +O] = Machine[F, I Is ?, O]
}

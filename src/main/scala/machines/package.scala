import machines.descriptor.Is

package object machines {
  type Process[-I, +F[_], +O] = Machine[I Is ?, F, O]
}

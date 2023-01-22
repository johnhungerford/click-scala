package click.util

object Types:
    type Label = String & Singleton
    type CharLabel = Char & Singleton
    type Maybe[T] = T | Unit

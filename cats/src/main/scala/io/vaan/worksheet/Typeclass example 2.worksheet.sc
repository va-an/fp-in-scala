case class Pc(
    cpu: String,
    ram: Int,
    maxRam: Int
)

case class Laptop(
    weight: Double,
    ram: Int,
    isRamReplaceable: Boolean
)

trait CustomizeComputer[A] {
    def replaceRam(computer: A, newRamSize: Int): Either[Throwable, A]
}

object CustomizeComputer {
    implicit val cCustomize: CustomizeComputer[Pc] =
        new CustomizeComputer[Pc] {
            override def replaceRam(computer: Pc, newRamSize: Int): Either[Throwable, Pc] = 
                if (newRamSize < computer.maxRam) Right(computer.copy(ram = newRamSize)) 
                else Left(new Throwable("too much RAM for PC")) 
        }

    implicit val LaptopCustomize: CustomizeComputer[Laptop] =
        new CustomizeComputer[Laptop] {
            override def replaceRam(computer: Laptop, newRamSize: Int): Either[Throwable, Laptop] = 
                if (computer.isRamReplaceable) Right(computer.copy(ram = newRamSize))
                else Left(new Throwable("RAM not replaceable for this laptop"))
        }

    def replaceRam[A](computer: A, newRamSize: Int)(implicit c: CustomizeComputer[A]): Either[Throwable, A] =
        c.replaceRam(computer, newRamSize)
}

object CustomizeComputerSyntax {
    implicit class CustomizeOps[A](computer: A)(implicit c: CustomizeComputer[A]) {
        def replaceRam(newRamSize: Int): Either[Throwable, A] =
            c.replaceRam(computer, newRamSize)
    }
}

val pc = Pc(cpu = "i5", ram = 4, maxRam = 16)
val laptop = Laptop(weight = 1.75, ram = 8, isRamReplaceable = false)

// interface objects
CustomizeComputer.replaceRam(pc, 32)
CustomizeComputer.replaceRam(pc, 8)

CustomizeComputer.replaceRam(laptop, 4)

// interface syntax
import CustomizeComputerSyntax._

pc.replaceRam(12)
pc.replaceRam(24)

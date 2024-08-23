
module InventarioModule
    implicit none

    type :: Equipo
        character(len=256) :: nombre
        character(len=256) :: ubicacion
        integer :: cantidad
        real :: precio_unitario
        contains
            procedure :: inicializar
            procedure :: agregarStock
            procedure :: quitarStock
        end type Equipo

    contains
    ! Inicializar equipos
    subroutine inicializar(this, nombre, ubicacion, cantidad, precio_unitario)
        class(Equipo), intent(inout) :: this
        character(len=256), intent(in) :: nombre, ubicacion
        integer, intent(in) :: cantidad
        real, intent(in) :: precio_unitario

        this%nombre = nombre
        this%ubicacion = ubicacion
        this%cantidad = cantidad
        this%precio_unitario = precio_unitario
    end subroutine inicializar

    ! Agregar stock
    subroutine agregarStock(this, cantidad)
        class(Equipo), intent(inout) :: this
        integer, intent(in) :: cantidad

        this%cantidad = this%cantidad + cantidad
    end subroutine agregarStock

    ! Quitar stock
    subroutine quitarStock(this, cantidad)
        class(Equipo), intent(inout) :: this
        integer, intent(in) :: cantidad
        integer :: resultante

        resultante = this%cantidad - cantidad
        if (resultante >= 0) then
            this%cantidad = resultante
        else
            print *, "No se puede eliminar esa cantidad, stock insuficiente."
        endif
    end subroutine quitarStock

end module InventarioModule

module global_vars
    use InventarioModule
    implicit none
    integer :: n=1
    type(Equipo), dimension(100) :: inventario
end module global_vars

program main
    use InventarioModule
    use global_vars
    implicit none
    integer :: op

    print *, "---------BIENVENIDO :D---------"
    do
        print *, "---------MENU---------"
        print *, "1. Cargar Inventario Inicial"
        print *, "2. Cargar Instrucciones de Movimientos"
        print *, "3. Crear Informe de Inventario"
        print *, "4. Salir"
        print *, "Selecciona una opcion:"
        read *, op

        select case (op)
            case (1)
                call cargarInventarioInicial()
            case (2)
                call cargarInstruccionesMovimientos()
            case (3)
                call crearInforme()
            case (4)
                print *, "Hasta Pronto :D"
                stop
            case default
                print *, "Opcion no valida :(, presione una tecla para continuar"
                read *
        end select
        call system("cls") ! cls -> windows | clear -> linux
    end do
end program main

subroutine cargarInventarioInicial()
    use InventarioModule
    use global_vars
    integer :: iunit, ios, pos, cantidad_int
    real :: precio_real
    character(len=256) :: nombre, ubicacion, cantidad, precio, linea, comando
    character(len=100) :: nombre_archivo

    iunit = 10

    print*, "Ingresa el nombre del archivo que quieres abrir con .inv :)"

    read(*,*) nombre_archivo

    open(unit=iunit, file=trim(nombre_archivo), status="old", action="read", iostat=ios)
    if (ios /= 0) then
        print *, "Error no se puede abrir el archivo de inventario :("
        return
    endif

    do
        read(iunit, '(A)', iostat=ios) linea
        if (ios /= 0) exit
        linea = trim(linea)

        pos = index(linea, ' ')
        if (pos > 0) then
            comando = linea(1:pos-1)
            linea = trim(linea(pos+1:))

            pos = index(linea, ';')
            if (pos > 0) then
                nombre = linea(1:pos-1)
                linea = trim(linea(pos+1:))

                pos = index(linea, ';')
                if (pos > 0) then
                    cantidad = linea(1:pos-1)
                    linea = trim(linea(pos+1:))

                    pos = index(linea, ';')
                    if (pos > 0) then
                        precio = linea(1:pos-1)
                        ubicacion = trim(linea(pos+1:))

                        read(cantidad, '(I10)', iostat=ios) cantidad_int
                        read(precio, '(F10.2)', iostat=ios) precio_real

                        if (comando == "crear_equipo") then
                            call crearEquipo(nombre, ubicacion, cantidad_int, precio_real)
                        endif
                    endif
                endif
            endif
        endif
    end do

    close(unit=iunit)
    print *, "Tu inventario inicial cargado con exito :D, Presione una tecla para continuar."
    read *
end subroutine cargarInventarioInicial

subroutine crearEquipo(nombre, ubicacion, cantidad, precio_unitario)
    use InventarioModule
    use global_vars
    character(len=256), intent(in) :: nombre, ubicacion
    integer, intent(in) :: cantidad
    real, intent(in) :: precio_unitario

    type(Equipo) :: nuevoEquipo
    call nuevoEquipo%inicializar(nombre, ubicacion, cantidad, precio_unitario)
    inventario(n) = nuevoEquipo
    n = n + 1
end subroutine crearEquipo

subroutine cargarInstruccionesMovimientos()
    use InventarioModule
    use global_vars
    integer :: iunit, ios, pos, cantidad_int
    character(len=256) :: linea, comando, nombre, ubicacion, cantidad
    character(len=100) :: nombre_archivo_acciones

    iunit = 11

    print*, "Ingresa el nombre del archivo que quieres abrir con .mov"
    read(*,*) nombre_archivo_acciones

    open(unit=iunit, file=trim(nombre_archivo_acciones), status="old", action="read", iostat=ios)
    if (ios /= 0) then
        print *, "Error no se puede abrir el archivo de movimientos :("
        return
    endif

    do
        read(iunit, '(A)', iostat=ios) linea
        if (ios /= 0) exit
        linea = trim(linea)

        pos = index(linea, ' ')
        if (pos > 0) then
            comando = linea(1:pos-1)
            linea = trim(linea(pos+1:))

            pos = index(linea, ';')
            if (pos > 0) then
                nombre = linea(1:pos-1)
                linea = trim(linea(pos+1:))

                pos = index(linea, ';')
                if (pos > 0) then
                    cantidad = linea(1:pos-1)
                    ubicacion = trim(linea(pos+1:))
                    read(cantidad, '(I10)', iostat=ios) cantidad_int

                    if (comando == "agregar_stock") then
                        call aumentarStock(nombre, ubicacion, cantidad_int)
                    else if (comando == "eliminar_equipo") then
                        call borraStock(nombre, ubicacion, cantidad_int)
                    endif
                endif
            endif
        endif
    end do

    close(unit=iunit)
    print *, "Las instrucciones cargaron correctamente :D, Presione una tecla para continuar."
    read *
end subroutine cargarInstruccionesMovimientos

subroutine aumentarStock(nombre, ubicacion, cantidad)
    use InventarioModule
    use global_vars
    character(len=256), intent(in) :: nombre, ubicacion
    integer, intent(in) :: cantidad

    integer :: i
    logical :: encontrado = .false.

    do i = 1, n-1
        if (inventario(i)%nombre == nombre .and. inventario(i)%ubicacion == ubicacion) then
            call inventario(i)%agregarStock(cantidad)
            encontrado = .true.
        endif
    end do

    if (.not. encontrado) then
        print *, "Error: El equipo no se encuentra en la ubicacion indicada :("
    endif
end subroutine aumentarStock

subroutine borraStock(nombre, ubicacion, cantidad)
    use InventarioModule
    use global_vars
    character(len=256), intent(in) :: nombre, ubicacion
    integer, intent(in) :: cantidad

    integer :: i
    logical :: encontrado = .false.

    do i = 1, n-1
        if (inventario(i)%nombre == nombre .and. inventario(i)%ubicacion == ubicacion) then
            call inventario(i)%quitarStock(cantidad)
            encontrado = .true.
        endif
    end do

    if (.not. encontrado) then
        print *, "Error: El equipo no se encuentra en la ubicacion indicada :("
    endif
end subroutine borraStock

subroutine crearInforme()
    use InventarioModule
    use global_vars
    integer :: iunit, ios, i

    iunit = 20
    open(unit=iunit, file="informe.txt", status="replace", action="write", iostat=ios)
    if (ios /= 0) then
        print *, "Tu informe no se a podido abrir :("
        stop
    endif

    write(iunit, '(A30, A10, A15, A15, A30)') "Equipo", "Cantidad", "Precio Unitario", "Valor Total", "Ubicación"

    do i = 1, n-1
        write(iunit, '(A30, I10, F15.2, F15.2, A30)') trim(inventario(i)%nombre), inventario(i)%cantidad, inventario(i)%precio_unitario, &
       inventario(i)%cantidad * inventario(i)%precio_unitario, trim(inventario(i)%ubicacion)
    end do

    close(iunit)
    print *, "Tu informe se a creado :D"
    print *, "Presiona una tecla para continuar"
    read *

end subroutine crearInforme
program main ! Интерполяция сплайнами с использованием библиотеки FGSL
use Task ! Модуль с процедурами
implicit none

     real(fgsl_double), allocatable, dimension(:) :: x_array, y_array ! Входные массивы
     real(fgsl_double) x ! Аргумент, при котором будем искать значение аппроксимирующей функции
     
     integer n ! Размер входных массивов
     integer ier ! Вспомогательная переменная

     ! Считывание размера входных массивов
     read(*,'()'); read(*,*) n
     
     ! Считывание аргумента, при котором будем искать значение аппроксимирующей функции
     read(*,'(/)'); read(*,*) x

     ! Выделение памяти под массивы
     allocate(x_array(n), stat = ier)
     if (ier .ne. 0) stop 'Не могу выделить память для массива x_array'
     
     allocate(y_array(n), stat = ier)
     if (ier .ne. 0) stop 'Не могу выделить память для массива y_array'

     ! Считывание входных массивов
     read(*,'(/)'); read(*,*) x_array
     read(*,'(/)'); read(*,*) y_array

     ! Вызов функции, вывод результата
     write(*,'(/,f10.5,1X,/)') GetSplineValue(x, x_array, y_array)

     deallocate(x_array, y_array)

end

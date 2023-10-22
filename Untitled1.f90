program circuits
    call menu
end program circuits

subroutine menu
    integer :: choice
    write(*,*) "Helloooo !!"
    write(*,*) "Choisissez la methode pour demarrer le programme"
    write(*,*) "1- Ouvrir un fichier existant"
    write(*,*) "2- Entrer manuellement les donnees"
    read(*, *) choice
    if (choice == 1) then
        ! Ouvrez le fichier
    else if (choice == 2) then
        call manuellement
    else
        write(*,*) "Choix invalide. Reessayez."
        ! Revenir au menu
    end if
end subroutine menu

subroutine manuellement
    real :: V0, r, sum_inv, req
    integer :: num_r, i, num_noeuds, j, k, n_serie, n_parallele
    integer, dimension(:), allocatable :: noeuds, r_serie
    real, dimension(:), allocatable :: r_parallele

    write(*,*) "Entrez la valeur de V0"
    read(*, *) V0
    write(*,*) "Combien de noeuds le circuit aura-t-il ?"
    read(*,*) num_noeuds
    write (*,*) "Combien de resistances y aura-t-il ?"
    read (*,*) num_r

    allocate(noeuds(num_noeuds))
    allocate(r_serie(num_r))
    allocate(r_parallele(num_r))  ! Alocado aqui fora do loop

    do i = 1, num_r
        write(*,*) "Résistance ", i
        write(*,*) "Entrez la valeur de la resistance (en ohms) :"
        read(*, *) r
        write(*,*) "Entrez les noeuds (separes par un espace) entre lesquels la resistance est connectee :"
        read(*, *) noeuds

        ! On a la valeur de la résistance et la matrice 'noeuds' contenant les nœuds.
        ! Faire quelque chose pour stocker ou les traiter.

        ! Vérifiez si elle est en série ou en parallèle
        n_serie = 0
        n_parallele = 0

        do k = 1, i - 1
            if (all(noeuds == noeuds(k))) then
                ! Elle est en série avec une résistance précédente
                r_serie(n_serie + 1) = i
                n_serie = n_serie + 1
            else if (any(noeuds == noeuds(k))) then
                ! Elle est en parallèle avec une résistance précédente
                n_parallele = n_parallele + 1
                r_parallele(n_parallele) = r  ! Atribuído o valor de r ao array
            end if
        end do

        if (n_serie > 0) then
        write(*,*) "Resistance ", i, " est en serie avec la (les) resistance(s) ", r_serie(1:n_serie)
        else if (n_parallele > 0) then
        !Calculo da resistencia equivalent em paralelo
            sum_inv = 0.0
                do j = 1, n_parallele
                    sum_inv = sum_inv + 1.0 / r_parallele(j)
                end do

        req = 1.0 / sum_inv

        write(*,*) "La resistance equivalente en parallele est : ", req
    end if

    deallocate(noeuds)
    deallocate(r_serie)
    deallocate(r_parallele)

end subroutine manuellement

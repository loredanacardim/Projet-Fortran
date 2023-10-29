program circuits
    implicit none
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
    !real :: V0, r, sum_inv, req
    !integer :: num_r, i,  j, k, n_serie, n_parallele
    !integer, dimension(:), allocatable :: noeuds, r_serie
    !real, dimension(:), allocatable :: r_parallele
    !real, dimension(:), allocatable :: corrente_sources, tensao_sources

    call noeuds_1
    call sources
    call elements

end subroutine manuellement

subroutine noeuds_1
    integer :: num_noeuds
    integer, dimension(:), allocatable :: noeuds
    ! Numero de Nós
    write(*,*) "Combien de noeuds le circuit aura-t-il ?"
    read(*,*) num_noeuds

    ! allocate(noeuds(num_noeuds))
    !allocate(r_serie(num_r))
    !allocate(r_parallele(num_r))  ! Alocado aqui fora do loop
end subroutine

subroutine sources
    integer :: num_sources, m, choix
    integer, dimension(:), allocatable :: noeuds
    real, dimension(:), allocatable :: corrente_sources, tensao_sources

    ! Nombre de sources
    write(*,*) "Combien de sources le circuit aura-t-il ?"
    read(*,*) num_sources

    ! Valeurs des sources
    do m = 1, num_sources
        write(*,*) "1- Courant ou 2- Tension : "
        read(*,*) choix
        if (choix == 1) then
            write(*,*) "Entrez la valeur du courant en A :"
            read(*,*) corrente_sources(m)
        else if (choix == 2) then
            write(*,*) "Entrez la valeur de la tension en V :"
            read(*, *) tensao_sources(m)
        else
            write(*,*) "Choix invalide. Réessayez."
            ! m = m - 1 ! Pour permettre à l'utilisateur de corriger le choix invalide
        end if
    end do
end subroutine


subroutine elements
    real :: V0, r, sum_inv, req
    integer :: num_r, i,  j, k, n_serie, n_parallele
    integer, dimension(:), allocatable :: noeuds, r_serie
    real, dimension(:), allocatable :: r_parallele
    real, dimension(:), allocatable :: corrente_sources, tensao_sources

     write (*,*) "Combien de resistances y aura-t-il ?"
    read (*,*) num_r
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
    end do
end subroutine

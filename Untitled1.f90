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
    real :: V0, r
    integer :: num_r, i, num_noeuds, j, k, n_serie, n_parallele
    integer, dimension(:), allocatable :: noeuds, r_serie, r_parallele

    write(*,*) "Entrez la valeur de V0"
    read(*, *) V0
    write(*,*) "Combien de noeuds le circuit aura-t-il ?"
    read(*,*) num_noeuds
    write (*,*) "Combien de resistances y aura-t-il ?"
    read (*,*) num_r

    allocate(noeuds(num_noeuds))
    allocate(r_serie(num_r))
    allocate(r_parallele(num_r))

    do i = 1, num_r
        write(*,*) "R�sistance ", i
        write(*,*) "Entrez la valeur de la r�sistance (en ohms) :"
        read(*, *) r
        write(*,*) "Entrez les n�uds (s�par�s par un espace) entre lesquels la r�sistance est connect�e :"
        read(*, *) noeuds

        ! On a la valeur de la r�sistance et la matrice 'noeuds' contenant les n�uds.
        ! Faire quelque chose pour stocker ou les traiter.

        ! V�rifiez si elle est en s�rie ou en parall�le
        n_serie = 0
        n_parallele = 0

        do k = 1, i - 1
            if (all(noeuds == noeuds(k))) then
                ! Elle est en s�rie avec une r�sistance pr�c�dente
                r_serie(n_serie + 1) = i
                n_serie = n_serie + 1
            else if (any(noeuds == noeuds(k))) then
                ! Elle est en parall�le avec une r�sistance pr�c�dente
                r_parallele(n_parallele + 1) = i
                n_parallele = n_parallele + 1
            end if
        end do

        if (n_serie > 0) then
            write(*,*) "Resistance ", i, " est en s�rie avec la (les) resistance(s) ", r_serie(1:n_serie)
        else if (n_parallele > 0) then
            write(*,*) "Resistance ", i, " est en parallele avec la (les) resistance(s) ", r_parallele(1:n_parallele)
        end if

    end do

    deallocate(noeuds)
    deallocate(r_serie)
    deallocate(r_parallele)
    ! Reste de votre programme
    ! ...
end subroutine manuellement

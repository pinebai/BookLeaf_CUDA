V30 :0x4 typh_quant_mod
28 ../src/comms/t3_quantity.f90 S624 0
05/11/2017  17:52:22
use typh_register_mod public 0 direct
use typh_types_mod public 0 direct
use mpi public 0 direct
use iso_fortran_env private
use iso_c_binding private
use typh_util_mod private
enduse
D 56 21 6 1 3 32 0 0 0 0 0
 0 32 3 3 32 32
D 59 21 6 1 3 32 0 0 0 0 0
 0 32 3 3 32 32
D 62 21 6 1 3 32 0 0 0 0 0
 0 32 3 3 32 32
D 65 21 6 1 3 32 0 0 0 0 0
 0 32 3 3 32 32
D 68 21 6 1 3 55 0 0 0 0 0
 0 55 3 3 55 55
D 71 21 6 1 3 55 0 0 0 0 0
 0 55 3 3 55 55
D 74 24 684 8 683 7
D 83 24 687 8 686 7
D 2537 24 7441 216 7440 7
D 2564 20 6
D 2566 20 6
D 2568 20 2537
D 2570 20 2537
D 2572 20 2537
D 2577 24 7470 48 7469 7
D 2589 20 2537
D 2591 20 2537
D 2596 24 7488 64 7487 7
D 2614 20 6
D 2616 20 6
D 2618 20 2577
D 2620 20 2596
D 2625 24 7514 40 7513 7
D 2640 20 2537
D 2642 20 6
D 2644 20 7
D 2649 24 7532 984 7531 7
D 2721 20 6
D 2723 20 6
D 2725 20 2625
D 2824 24 7608 32 7607 3
D 2833 24 7618 48 7617 3
D 2951 24 7488 64 7487 7
D 2977 24 7532 984 7531 7
D 3001 24 7785 208 7778 7
D 3021 20 2951
D 3023 20 2977
D 3028 24 7810 384 7779 7
D 3052 20 6
D 3057 24 7844 696 7843 7
D 3072 20 6
D 3074 20 3057
D 3139 21 9 1 1444 1450 0 1 0 0 1
 1445 1448 1449 1445 1448 1446
D 3142 21 6 1 0 172 0 0 0 0 0
 0 172 0 3 172 0
D 3145 21 9 2 1452 1463 0 1 0 0 1
 1453 1456 1457 1453 1456 1454
 1458 1461 1462 1458 1461 1459
D 3148 21 6 1 0 207 0 0 0 0 0
 0 207 0 3 207 0
D 3151 21 6 1 1465 1471 0 1 0 0 1
 1466 1469 1470 1466 1469 1467
D 3154 21 6 1 0 172 0 0 0 0 0
 0 172 0 3 172 0
D 3157 21 6 2 1473 1484 0 1 0 0 1
 1474 1477 1478 1474 1477 1475
 1479 1482 1483 1479 1482 1480
D 3160 21 6 1 0 207 0 0 0 0 0
 0 207 0 3 207 0
S 624 24 0 0 0 8 1 0 5011 10005 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 19 0 0 0 0 0 0 typh_quant_mod
S 628 23 0 0 0 8 7734 624 5073 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ty_errorcheck
S 629 23 0 0 0 8 7740 624 5087 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ty_memcheck
S 632 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 634 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 635 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 636 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 640 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 641 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 663 7 22 iso_fortran_env integer_kinds$ac
R 665 7 24 iso_fortran_env logical_kinds$ac
R 667 7 26 iso_fortran_env real_kinds$ac
R 683 25 6 iso_c_binding c_ptr
R 684 5 7 iso_c_binding val c_ptr
R 686 25 9 iso_c_binding c_funptr
R 687 5 10 iso_c_binding val c_funptr
R 721 6 44 iso_c_binding c_null_ptr$ac
R 723 6 46 iso_c_binding c_null_funptr$ac
R 724 26 47 iso_c_binding ==
R 726 26 49 iso_c_binding !=
S 753 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 764 3 0 0 0 16 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16
S 767 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 768 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 769 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 771 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 774 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 777 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 23 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 778 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 795 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 19 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 7371 3 0 0 0 7 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -999 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7
R 7440 25 52 typh_types_mod keyll_tp
R 7441 5 53 typh_types_mod layer keyll_tp
R 7442 5 54 typh_types_mod proc keyll_tp
R 7444 5 56 typh_types_mod list keyll_tp
R 7445 5 57 typh_types_mod list$sd keyll_tp
R 7446 5 58 typh_types_mod list$p keyll_tp
R 7447 5 59 typh_types_mod list$o keyll_tp
R 7449 5 61 typh_types_mod nlist keyll_tp
R 7451 5 63 typh_types_mod blocklens keyll_tp
R 7452 5 64 typh_types_mod blocklens$sd keyll_tp
R 7453 5 65 typh_types_mod blocklens$p keyll_tp
R 7454 5 66 typh_types_mod blocklens$o keyll_tp
R 7456 5 68 typh_types_mod next keyll_tp
R 7458 5 70 typh_types_mod next$p keyll_tp
R 7460 5 72 typh_types_mod prev keyll_tp
R 7462 5 74 typh_types_mod prev$p keyll_tp
R 7464 5 76 typh_types_mod parent keyll_tp
R 7466 5 78 typh_types_mod parent$p keyll_tp
R 7469 25 81 typh_types_mod key_set_tp
R 7470 5 82 typh_types_mod centring key_set_tp
R 7471 5 83 typh_types_mod auxid key_set_tp
R 7472 5 84 typh_types_mod stride key_set_tp
R 7473 5 85 typh_types_mod lmin key_set_tp
R 7474 5 86 typh_types_mod lmax key_set_tp
R 7475 5 87 typh_types_mod partitionid key_set_tp
R 7476 5 88 typh_types_mod nsend key_set_tp
R 7477 5 89 typh_types_mod nrecv key_set_tp
R 7478 5 90 typh_types_mod send_keys key_set_tp
R 7480 5 92 typh_types_mod send_keys$p key_set_tp
R 7482 5 94 typh_types_mod recv_keys key_set_tp
R 7484 5 96 typh_types_mod recv_keys$p key_set_tp
R 7487 25 99 typh_types_mod v3_comm_quant_tp
R 7488 5 100 typh_types_mod quantid v3_comm_quant_tp
R 7489 5 101 typh_types_mod receivequantid v3_comm_quant_tp
R 7490 5 102 typh_types_mod keysetid v3_comm_quant_tp
R 7491 5 103 typh_types_mod ghostsmin v3_comm_quant_tp
R 7492 5 104 typh_types_mod ghostsmax v3_comm_quant_tp
R 7493 5 105 typh_types_mod quantsize v3_comm_quant_tp
R 7494 5 106 typh_types_mod nrepeat v3_comm_quant_tp
R 7495 5 107 typh_types_mod stride v3_comm_quant_tp
R 7496 5 108 typh_types_mod oldmpitype v3_comm_quant_tp
R 7498 5 110 typh_types_mod oldmpitype$p v3_comm_quant_tp
R 7500 5 112 typh_types_mod newmpitype v3_comm_quant_tp
R 7502 5 114 typh_types_mod newmpitype$p v3_comm_quant_tp
R 7504 5 116 typh_types_mod keyset v3_comm_quant_tp
R 7506 5 118 typh_types_mod keyset$p v3_comm_quant_tp
R 7508 5 120 typh_types_mod next v3_comm_quant_tp
R 7510 5 122 typh_types_mod next$p v3_comm_quant_tp
R 7513 25 125 typh_types_mod v3_schedulepart_tp
R 7514 5 126 typh_types_mod new_mpi_tp v3_schedulepart_tp
R 7515 5 127 typh_types_mod quantsize v3_schedulepart_tp
R 7516 5 128 typh_types_mod nrepeat v3_schedulepart_tp
R 7517 5 129 typh_types_mod stride v3_schedulepart_tp
R 7518 5 130 typh_types_mod key v3_schedulepart_tp
R 7520 5 132 typh_types_mod key$p v3_schedulepart_tp
R 7522 5 134 typh_types_mod old_mpi_tp v3_schedulepart_tp
R 7524 5 136 typh_types_mod old_mpi_tp$p v3_schedulepart_tp
R 7526 5 138 typh_types_mod address v3_schedulepart_tp
R 7528 5 140 typh_types_mod address$p v3_schedulepart_tp
R 7531 25 143 typh_types_mod v3_schedule_tp
R 7532 5 144 typh_types_mod nsend v3_schedule_tp
R 7534 5 146 typh_types_mod send_proc v3_schedule_tp
R 7535 5 147 typh_types_mod send_proc$sd v3_schedule_tp
R 7536 5 148 typh_types_mod send_proc$p v3_schedule_tp
R 7537 5 149 typh_types_mod send_proc$o v3_schedule_tp
R 7540 5 152 typh_types_mod mpi_send_tp v3_schedule_tp
R 7541 5 153 typh_types_mod mpi_send_tp$sd v3_schedule_tp
R 7542 5 154 typh_types_mod mpi_send_tp$p v3_schedule_tp
R 7543 5 155 typh_types_mod mpi_send_tp$o v3_schedule_tp
R 7546 5 158 typh_types_mod send_requests v3_schedule_tp
R 7547 5 159 typh_types_mod send_requests$sd v3_schedule_tp
R 7548 5 160 typh_types_mod send_requests$p v3_schedule_tp
R 7549 5 161 typh_types_mod send_requests$o v3_schedule_tp
R 7552 5 164 typh_types_mod send_nparts v3_schedule_tp
R 7553 5 165 typh_types_mod send_nparts$sd v3_schedule_tp
R 7554 5 166 typh_types_mod send_nparts$p v3_schedule_tp
R 7555 5 167 typh_types_mod send_nparts$o v3_schedule_tp
R 7558 5 170 typh_types_mod send_start v3_schedule_tp
R 7559 5 171 typh_types_mod send_start$sd v3_schedule_tp
R 7560 5 172 typh_types_mod send_start$p v3_schedule_tp
R 7561 5 173 typh_types_mod send_start$o v3_schedule_tp
R 7563 5 175 typh_types_mod nrecv v3_schedule_tp
R 7565 5 177 typh_types_mod recv_proc v3_schedule_tp
R 7566 5 178 typh_types_mod recv_proc$sd v3_schedule_tp
R 7567 5 179 typh_types_mod recv_proc$p v3_schedule_tp
R 7568 5 180 typh_types_mod recv_proc$o v3_schedule_tp
R 7571 5 183 typh_types_mod mpi_recv_tp v3_schedule_tp
R 7572 5 184 typh_types_mod mpi_recv_tp$sd v3_schedule_tp
R 7573 5 185 typh_types_mod mpi_recv_tp$p v3_schedule_tp
R 7574 5 186 typh_types_mod mpi_recv_tp$o v3_schedule_tp
R 7577 5 189 typh_types_mod recv_requests v3_schedule_tp
R 7578 5 190 typh_types_mod recv_requests$sd v3_schedule_tp
R 7579 5 191 typh_types_mod recv_requests$p v3_schedule_tp
R 7580 5 192 typh_types_mod recv_requests$o v3_schedule_tp
R 7583 5 195 typh_types_mod recv_nparts v3_schedule_tp
R 7584 5 196 typh_types_mod recv_nparts$sd v3_schedule_tp
R 7585 5 197 typh_types_mod recv_nparts$p v3_schedule_tp
R 7586 5 198 typh_types_mod recv_nparts$o v3_schedule_tp
R 7589 5 201 typh_types_mod recv_start v3_schedule_tp
R 7590 5 202 typh_types_mod recv_start$sd v3_schedule_tp
R 7591 5 203 typh_types_mod recv_start$p v3_schedule_tp
R 7592 5 204 typh_types_mod recv_start$o v3_schedule_tp
R 7595 5 207 typh_types_mod parts v3_schedule_tp
R 7596 5 208 typh_types_mod parts$sd v3_schedule_tp
R 7597 5 209 typh_types_mod parts$p v3_schedule_tp
R 7598 5 210 typh_types_mod parts$o v3_schedule_tp
R 7607 25 6 typh_util_mod mptypes_tp
R 7608 5 7 typh_util_mod real mptypes_tp
R 7609 5 8 typh_util_mod integer mptypes_tp
R 7610 5 9 typh_util_mod logical mptypes_tp
R 7611 5 10 typh_util_mod character mptypes_tp
R 7612 5 11 typh_util_mod mem mptypes_tp
R 7613 5 12 typh_util_mod size mptypes_tp
R 7614 5 13 typh_util_mod mpi mptypes_tp
R 7615 5 14 typh_util_mod integerpad mptypes_tp
R 7617 25 16 typh_util_mod mp_tp
R 7618 5 17 typh_util_mod comm mp_tp
R 7619 5 18 typh_util_mod info mp_tp
R 7620 5 19 typh_util_mod size mp_tp
R 7621 5 20 typh_util_mod rank mp_tp
R 7622 5 21 typh_util_mod minrank mp_tp
R 7623 5 22 typh_util_mod maxrank mp_tp
R 7624 5 23 typh_util_mod masterrank mp_tp
R 7625 5 24 typh_util_mod error mp_tp
R 7626 5 25 typh_util_mod ismaster mp_tp
R 7627 5 26 typh_util_mod initialised mp_tp
R 7628 5 27 typh_util_mod finalised mp_tp
R 7629 5 28 typh_util_mod logicalpad mp_tp
R 7734 14 133 typh_util_mod ty_errorcheck
R 7740 14 139 typh_util_mod ty_memcheck
R 7778 25 16 typh_register_mod phase_tp
R 7779 25 17 typh_register_mod quant_tp
R 7785 5 23 typh_register_mod nghosts phase_tp
R 7786 5 24 typh_register_mod nquants phase_tp
R 7787 5 25 typh_register_mod pure phase_tp
R 7788 5 26 typh_register_mod name phase_tp
R 7790 5 28 typh_register_mod quantlist phase_tp
R 7791 5 29 typh_register_mod quantlist$sd phase_tp
R 7792 5 30 typh_register_mod quantlist$p phase_tp
R 7793 5 31 typh_register_mod quantlist$o phase_tp
R 7795 5 33 typh_register_mod keysetid phase_tp
R 7796 5 34 typh_register_mod ghostsmin phase_tp
R 7797 5 35 typh_register_mod ghostsmax phase_tp
R 7798 5 36 typh_register_mod pqinfo phase_tp
R 7800 5 38 typh_register_mod pqinfo$p phase_tp
R 7802 5 40 typh_register_mod schedule phase_tp
R 7804 5 42 typh_register_mod schedule$p phase_tp
R 7806 5 44 typh_register_mod isbuilt phase_tp
R 7807 5 45 typh_register_mod iscommit phase_tp
R 7810 5 48 typh_register_mod qdataid quant_tp
R 7811 5 49 typh_register_mod nghosts quant_tp
R 7812 5 50 typh_register_mod centring quant_tp
R 7813 5 51 typh_register_mod datatype quant_tp
R 7814 5 52 typh_register_mod pure quant_tp
R 7815 5 53 typh_register_mod aux quant_tp
R 7816 5 54 typh_register_mod name quant_tp
R 7817 5 55 typh_register_mod mpi_datatype quant_tp
R 7818 5 56 typh_register_mod auxid quant_tp
R 7819 5 57 typh_register_mod rank quant_tp
R 7821 5 59 typh_register_mod dims quant_tp
R 7822 5 60 typh_register_mod dims$sd quant_tp
R 7823 5 61 typh_register_mod dims$p quant_tp
R 7824 5 62 typh_register_mod dims$o quant_tp
R 7826 5 64 typh_register_mod meshdim quant_tp
R 7827 5 65 typh_register_mod stride quant_tp
R 7828 5 66 typh_register_mod quant_address quant_tp
R 7830 5 68 typh_register_mod lowbnd quant_tp
R 7831 5 69 typh_register_mod lowbnd$sd quant_tp
R 7832 5 70 typh_register_mod lowbnd$p quant_tp
R 7833 5 71 typh_register_mod lowbnd$o quant_tp
R 7836 5 74 typh_register_mod uppbnd quant_tp
R 7837 5 75 typh_register_mod uppbnd$sd quant_tp
R 7838 5 76 typh_register_mod uppbnd$p quant_tp
R 7839 5 77 typh_register_mod uppbnd$o quant_tp
R 7843 25 81 typh_register_mod pqll_tp
R 7844 5 82 typh_register_mod phase pqll_tp
R 7845 5 83 typh_register_mod quant pqll_tp
R 7847 5 85 typh_register_mod list pqll_tp
R 7848 5 86 typh_register_mod list$sd pqll_tp
R 7849 5 87 typh_register_mod list$p pqll_tp
R 7850 5 88 typh_register_mod list$o pqll_tp
R 7852 5 90 typh_register_mod nlist pqll_tp
R 7853 5 91 typh_register_mod next pqll_tp
R 7855 5 93 typh_register_mod next$p pqll_tp
S 8008 19 0 0 0 8 1 624 38946 4000 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 841 4 0 0 0 0 0 624 0 0 0 0 typh_set_quant_address
O 8008 4 8012 8011 8010 8009
S 8009 27 0 0 0 6 8022 624 38969 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 842 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 mupdatequant_r8_1d
Q 8009 8008 0
S 8010 27 0 0 0 6 8031 624 38988 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 843 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 mupdatequant_r8_2d
Q 8010 8008 0
S 8011 27 0 0 0 6 8040 624 39007 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 844 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 mupdatequant_i4_1d
Q 8011 8008 0
S 8012 27 0 0 0 6 8049 624 39026 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 845 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 mupdatequant_i4_2d
Q 8012 8008 0
S 8013 23 5 0 0 6 8020 624 39045 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 typh_add_quant_to_phase
S 8014 1 3 1 0 6 1 8013 38690 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 phaseid
S 8015 1 3 1 0 6 1 8013 35229 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 quantid
S 8016 1 3 1 0 6 1 8013 35237 80000004 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 receivequantid
S 8017 1 3 1 0 6 1 8013 35252 80000004 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 keysetid
S 8018 1 3 1 0 6 1 8013 35261 80000004 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ghostsmin
S 8019 1 3 1 0 6 1 8013 35271 80000004 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ghostsmax
S 8020 14 5 0 0 6 1 8013 39045 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5003 6 0 0 8021 0 0 0 0 0 0 0 0 0 36 0 624 0 0 0 0 typh_add_quant_to_phase
F 8020 6 8014 8015 8016 8017 8018 8019
S 8021 1 3 0 0 6 1 8013 39045 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 typh_add_quant_to_phase
S 8022 23 5 0 0 6 8025 624 38969 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mupdatequant_r8_1d
S 8023 1 3 1 0 6 1 8022 35229 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 quantid
S 8024 7 3 1 0 3139 1 8022 39069 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8027 0 0 0 0 0 0 0 0 pquant
S 8025 14 5 0 0 6 1 8022 38969 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5010 2 0 0 8026 0 0 0 0 0 0 0 0 0 167 0 624 0 0 0 0 mupdatequant_r8_1d
F 8025 2 8023 8024
S 8026 1 3 0 0 6 1 8022 38969 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mupdatequant_r8_1d
S 8027 8 1 0 0 3142 1 8022 39076 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pquant$sd
S 8031 23 5 0 0 6 8034 624 38988 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mupdatequant_r8_2d
S 8032 1 3 1 0 6 1 8031 35229 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 quantid
S 8033 7 3 1 0 3145 1 8031 39069 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8036 0 0 0 0 0 0 0 0 pquant
S 8034 14 5 0 0 6 1 8031 38988 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5013 2 0 0 8035 0 0 0 0 0 0 0 0 0 219 0 624 0 0 0 0 mupdatequant_r8_2d
F 8034 2 8032 8033
S 8035 1 3 0 0 6 1 8031 38988 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mupdatequant_r8_2d
S 8036 8 1 0 0 3148 1 8031 39118 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pquant$sd7
S 8040 23 5 0 0 6 8043 624 39007 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mupdatequant_i4_1d
S 8041 1 3 1 0 6 1 8040 35229 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 quantid
S 8042 7 3 1 0 3151 1 8040 39069 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8045 0 0 0 0 0 0 0 0 pquant
S 8043 14 5 0 0 6 1 8040 39007 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5016 2 0 0 8044 0 0 0 0 0 0 0 0 0 269 0 624 0 0 0 0 mupdatequant_i4_1d
F 8043 2 8041 8042
S 8044 1 3 0 0 6 1 8040 39007 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mupdatequant_i4_1d
S 8045 8 1 0 0 3154 1 8040 39165 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pquant$sd12
S 8049 23 5 0 0 6 8052 624 39026 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mupdatequant_i4_2d
S 8050 1 3 1 0 6 1 8049 35229 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 quantid
S 8051 7 3 1 0 3157 1 8049 39069 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8054 0 0 0 0 0 0 0 0 pquant
S 8052 14 5 0 0 6 1 8049 39026 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5019 2 0 0 8053 0 0 0 0 0 0 0 0 0 318 0 624 0 0 0 0 mupdatequant_i4_2d
F 8052 2 8050 8051
S 8053 1 3 0 0 6 1 8049 39026 4 1003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mupdatequant_i4_2d
S 8054 8 1 0 0 3160 1 8049 39215 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pquant$sd17
A 13 2 0 0 0 6 632 0 0 0 13 0 0 0 0 0 0 0 0 0 0
A 19 2 0 0 0 6 634 0 0 0 19 0 0 0 0 0 0 0 0 0 0
A 21 2 0 0 0 6 636 0 0 0 21 0 0 0 0 0 0 0 0 0 0
A 30 2 0 0 0 6 635 0 0 0 30 0 0 0 0 0 0 0 0 0 0
A 32 2 0 0 0 6 640 0 0 0 32 0 0 0 0 0 0 0 0 0 0
A 55 2 0 0 0 6 641 0 0 0 55 0 0 0 0 0 0 0 0 0 0
A 61 1 0 1 0 56 663 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 67 1 0 1 0 62 665 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 72 1 0 3 0 68 667 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 125 1 0 0 0 74 721 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 128 1 0 0 0 83 723 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 152 2 0 0 0 16 764 0 0 0 152 0 0 0 0 0 0 0 0 0 0
A 162 2 0 0 0 6 753 0 0 0 162 0 0 0 0 0 0 0 0 0 0
A 169 2 0 0 0 6 767 0 0 0 169 0 0 0 0 0 0 0 0 0 0
A 172 2 0 0 0 6 768 0 0 0 172 0 0 0 0 0 0 0 0 0 0
A 174 2 0 0 0 6 769 0 0 0 174 0 0 0 0 0 0 0 0 0 0
A 182 2 0 0 0 6 771 0 0 0 182 0 0 0 0 0 0 0 0 0 0
A 198 2 0 0 0 6 774 0 0 0 198 0 0 0 0 0 0 0 0 0 0
A 205 2 0 0 0 6 777 0 0 0 205 0 0 0 0 0 0 0 0 0 0
A 207 2 0 0 0 6 778 0 0 0 207 0 0 0 0 0 0 0 0 0 0
A 254 2 0 0 241 6 795 0 0 0 254 0 0 0 0 0 0 0 0 0 0
A 1217 2 0 0 811 7 7371 0 0 0 1217 0 0 0 0 0 0 0 0 0 0
A 1443 1 0 5 0 3142 8027 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1444 10 0 0 1208 6 1443 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1445 10 0 0 1444 6 1443 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1446 10 0 0 1445 6 1443 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1447 4 0 0 399 6 1446 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1448 4 0 0 1239 6 1445 0 1447 0 0 0 0 1 0 0 0 0 0 0 0
A 1449 10 0 0 1446 6 1443 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1450 10 0 0 1449 6 1443 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1451 1 0 7 1031 3148 8036 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1452 10 0 0 1229 6 1451 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1453 10 0 0 1452 6 1451 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1454 10 0 0 1453 6 1451 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1455 4 0 0 1000 6 1454 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1456 4 0 0 668 6 1453 0 1455 0 0 0 0 1 0 0 0 0 0 0 0
A 1457 10 0 0 1454 6 1451 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1458 10 0 0 1457 6 1451 16 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 254
A 1459 10 0 0 1458 6 1451 19 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 198
A 1460 4 0 0 948 6 1459 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1461 4 0 0 0 6 1458 0 1460 0 0 0 0 1 0 0 0 0 0 0 0
A 1462 10 0 0 1459 6 1451 22 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 205
A 1463 10 0 0 1462 6 1451 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1464 1 0 5 965 3154 8045 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1465 10 0 0 506 6 1464 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1466 10 0 0 1465 6 1464 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1467 10 0 0 1466 6 1464 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1468 4 0 0 421 6 1467 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1469 4 0 0 1372 6 1466 0 1468 0 0 0 0 1 0 0 0 0 0 0 0
A 1470 10 0 0 1467 6 1464 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1471 10 0 0 1470 6 1464 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1472 1 0 7 0 3160 8054 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1473 10 0 0 989 6 1472 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1474 10 0 0 1473 6 1472 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1475 10 0 0 1474 6 1472 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1476 4 0 0 1050 6 1475 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1477 4 0 0 1202 6 1474 0 1476 0 0 0 0 1 0 0 0 0 0 0 0
A 1478 10 0 0 1475 6 1472 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1479 10 0 0 1478 6 1472 16 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 254
A 1480 10 0 0 1479 6 1472 19 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 198
A 1481 4 0 0 1323 6 1480 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1482 4 0 0 0 6 1479 0 1481 0 0 0 0 1 0 0 0 0 0 0 0
A 1483 10 0 0 1480 6 1472 22 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 205
A 1484 10 0 0 1483 6 1472 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
Z
J 69 1 1
V 61 56 7 0
R 0 59 0 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 30 1
A 0 6 0 0 1 32 1
A 0 6 0 0 1 13 0
J 71 1 1
V 67 62 7 0
R 0 65 0 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 30 1
A 0 6 0 0 1 32 1
A 0 6 0 0 1 13 0
J 73 1 1
V 72 68 7 0
R 0 71 0 0
A 0 6 0 0 1 32 1
A 0 6 0 0 1 13 1
A 0 6 0 0 1 19 0
J 149 1 1
V 125 74 7 0
S 0 74 0 0 0
A 0 6 0 0 1 2 0
J 150 1 1
V 128 83 7 0
S 0 83 0 0 0
A 0 6 0 0 1 2 0
T 7440 2537 0 3 0 0
A 7446 7 2564 0 1 2 1
A 7447 7 0 0 1 2 1
A 7445 6 0 172 1 2 1
A 7453 7 2566 0 1 2 1
A 7454 7 0 0 1 2 1
A 7452 6 0 172 1 2 1
A 7458 7 2568 0 1 2 1
A 7462 7 2570 0 1 2 1
A 7466 7 2572 0 1 2 0
T 7469 2577 0 3 0 0
A 7480 7 2589 0 1 2 1
A 7484 7 2591 0 1 2 0
T 7487 2596 0 3 0 0
A 7498 7 2614 0 1 2 1
A 7502 7 2616 0 1 2 1
A 7506 7 2618 0 1 2 1
A 7510 7 2620 0 1 2 0
T 7513 2625 0 3 0 0
A 7520 7 2640 0 1 2 1
A 7524 7 2642 0 1 2 1
A 7528 7 2644 0 1 2 0
T 7531 2649 0 3 0 0
A 7548 7 2721 0 1 2 1
A 7549 7 0 0 1 2 1
A 7547 6 0 172 1 2 1
A 7579 7 2723 0 1 2 1
A 7580 7 0 0 1 2 1
A 7578 6 0 172 1 2 1
A 7597 7 2725 0 1 2 1
A 7598 7 0 0 1 2 1
A 7596 6 0 172 1 2 0
T 7607 2824 0 3 0 0
A 7608 6 0 0 1 2 1
A 7609 6 0 0 1 2 1
A 7610 6 0 0 1 2 1
A 7611 6 0 0 1 2 1
A 7612 6 0 0 1 2 1
A 7613 6 0 0 1 2 1
A 7614 6 0 0 1 2 0
T 7617 2833 0 3 0 0
A 7618 6 0 0 1 30 1
A 7619 6 0 0 1 2 1
A 7620 6 0 0 1 2 1
A 7621 6 0 0 1 21 1
A 7622 6 0 0 1 21 1
A 7623 6 0 0 1 21 1
A 7624 6 0 0 1 21 1
A 7625 6 0 0 1 2 1
A 7626 16 0 0 1 152 1
A 7627 16 0 0 1 152 1
A 7628 16 0 0 1 152 0
T 7778 3001 0 3 0 0
A 7800 7 3021 0 1 2 1
A 7804 7 3023 0 1 2 0
T 7779 3028 0 3 0 0
A 7823 7 3052 0 1 2 1
A 7824 7 0 0 1 2 1
A 7822 6 0 172 1 2 1
A 7828 7 0 0 1 1217 0
T 7843 3057 0 3 0 0
T 7844 3001 0 3 0 1
A 7800 7 3021 0 1 2 1
A 7804 7 3023 0 1 2 0
T 7845 3028 0 3 0 1
A 7823 7 3052 0 1 2 1
A 7824 7 0 0 1 2 1
A 7822 6 0 172 1 2 1
A 7828 7 0 0 1 1217 0
A 7849 7 3072 0 1 2 1
A 7850 7 0 0 1 2 1
A 7848 6 0 172 1 2 1
A 7855 7 3074 0 1 2 0
Z

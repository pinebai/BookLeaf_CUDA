V30 :0x4 typhon
23 ../src/comms/typhon.f90 S624 0
05/11/2017  17:52:22
use typh_decomposition_mod public 0 direct
use typh_schedule_mod public 0 indirect
use typh_exchange_mod public 0 direct
use typh_register_mod public 0 direct
use typh_quant_mod public 0 direct
use typh_collect_mod public 0 indirect
use mpi public 0 indirect
use typh_types_mod public 0 direct
use typh_util_mod public 0 direct
use typh_key_mod public 0 direct
use iso_fortran_env private
use iso_c_binding private
enduse
D 56 24 650 8 649 7
D 65 24 653 8 652 7
D 74 21 6 1 3 13 0 0 0 0 0
 0 13 3 3 13 13
D 77 21 6 1 3 13 0 0 0 0 0
 0 13 3 3 13 13
D 80 21 6 1 3 13 0 0 0 0 0
 0 13 3 3 13 13
D 83 21 6 1 3 13 0 0 0 0 0
 0 13 3 3 13 13
D 86 21 6 1 3 111 0 0 0 0 0
 0 111 3 3 111 111
D 89 21 6 1 3 111 0 0 0 0 0
 0 111 3 3 111 111
D 2537 24 7443 216 7442 7
D 2564 20 6
D 2566 20 6
D 2568 20 2537
D 2570 20 2537
D 2572 20 2537
D 2577 24 7472 48 7471 7
D 2589 20 2537
D 2591 20 2537
D 2596 24 7490 64 7489 7
D 2614 20 6
D 2616 20 6
D 2618 20 2577
D 2620 20 2596
D 2625 24 7516 40 7515 7
D 2640 20 2537
D 2642 20 6
D 2644 20 7
D 2649 24 7534 984 7533 7
D 2721 20 6
D 2723 20 6
D 2725 20 2625
D 2824 24 7610 32 7609 3
D 2833 24 7620 48 7619 3
D 2951 24 7490 64 7489 7
D 2977 24 7534 984 7533 7
D 3001 24 7787 208 7780 7
D 3021 20 2951
D 3023 20 2977
D 3028 24 7812 384 7781 7
D 3052 20 6
D 3057 24 7846 696 7845 7
D 3072 20 6
D 3074 20 3057
D 3245 24 8011 768 8010 7
D 3295 20 6
D 3297 20 6
D 3299 20 6
D 3301 20 6
D 3303 20 6
D 3305 20 6
D 3307 20 6
D 3312 24 8062 776 8061 7
D 3533 24 7472 48 7471 7
D 3659 24 8281 8 8280 7
D 3668 20 3533
S 624 24 0 0 0 8 1 0 5011 10005 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 31 0 0 0 0 0 0 0 0 19 0 0 0 0 0 0 typhon
S 633 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 634 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 635 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 649 25 6 iso_c_binding c_ptr
R 650 5 7 iso_c_binding val c_ptr
R 652 25 9 iso_c_binding c_funptr
R 653 5 10 iso_c_binding val c_funptr
R 687 6 44 iso_c_binding c_null_ptr$ac
R 689 6 46 iso_c_binding c_null_funptr$ac
R 690 26 47 iso_c_binding ==
R 692 26 49 iso_c_binding !=
S 721 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 722 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 726 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 748 7 22 iso_fortran_env integer_kinds$ac
R 750 7 24 iso_fortran_env logical_kinds$ac
R 752 7 26 iso_fortran_env real_kinds$ac
S 766 3 0 0 0 16 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16
S 770 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 780 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 7373 3 0 0 0 7 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -999 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7
R 7442 25 52 typh_types_mod keyll_tp
R 7443 5 53 typh_types_mod layer keyll_tp
R 7444 5 54 typh_types_mod proc keyll_tp
R 7446 5 56 typh_types_mod list keyll_tp
R 7447 5 57 typh_types_mod list$sd keyll_tp
R 7448 5 58 typh_types_mod list$p keyll_tp
R 7449 5 59 typh_types_mod list$o keyll_tp
R 7451 5 61 typh_types_mod nlist keyll_tp
R 7453 5 63 typh_types_mod blocklens keyll_tp
R 7454 5 64 typh_types_mod blocklens$sd keyll_tp
R 7455 5 65 typh_types_mod blocklens$p keyll_tp
R 7456 5 66 typh_types_mod blocklens$o keyll_tp
R 7458 5 68 typh_types_mod next keyll_tp
R 7460 5 70 typh_types_mod next$p keyll_tp
R 7462 5 72 typh_types_mod prev keyll_tp
R 7464 5 74 typh_types_mod prev$p keyll_tp
R 7466 5 76 typh_types_mod parent keyll_tp
R 7468 5 78 typh_types_mod parent$p keyll_tp
R 7471 25 81 typh_types_mod key_set_tp
R 7472 5 82 typh_types_mod centring key_set_tp
R 7473 5 83 typh_types_mod auxid key_set_tp
R 7474 5 84 typh_types_mod stride key_set_tp
R 7475 5 85 typh_types_mod lmin key_set_tp
R 7476 5 86 typh_types_mod lmax key_set_tp
R 7477 5 87 typh_types_mod partitionid key_set_tp
R 7478 5 88 typh_types_mod nsend key_set_tp
R 7479 5 89 typh_types_mod nrecv key_set_tp
R 7480 5 90 typh_types_mod send_keys key_set_tp
R 7482 5 92 typh_types_mod send_keys$p key_set_tp
R 7484 5 94 typh_types_mod recv_keys key_set_tp
R 7486 5 96 typh_types_mod recv_keys$p key_set_tp
R 7489 25 99 typh_types_mod v3_comm_quant_tp
R 7490 5 100 typh_types_mod quantid v3_comm_quant_tp
R 7491 5 101 typh_types_mod receivequantid v3_comm_quant_tp
R 7492 5 102 typh_types_mod keysetid v3_comm_quant_tp
R 7493 5 103 typh_types_mod ghostsmin v3_comm_quant_tp
R 7494 5 104 typh_types_mod ghostsmax v3_comm_quant_tp
R 7495 5 105 typh_types_mod quantsize v3_comm_quant_tp
R 7496 5 106 typh_types_mod nrepeat v3_comm_quant_tp
R 7497 5 107 typh_types_mod stride v3_comm_quant_tp
R 7498 5 108 typh_types_mod oldmpitype v3_comm_quant_tp
R 7500 5 110 typh_types_mod oldmpitype$p v3_comm_quant_tp
R 7502 5 112 typh_types_mod newmpitype v3_comm_quant_tp
R 7504 5 114 typh_types_mod newmpitype$p v3_comm_quant_tp
R 7506 5 116 typh_types_mod keyset v3_comm_quant_tp
R 7508 5 118 typh_types_mod keyset$p v3_comm_quant_tp
R 7510 5 120 typh_types_mod next v3_comm_quant_tp
R 7512 5 122 typh_types_mod next$p v3_comm_quant_tp
R 7515 25 125 typh_types_mod v3_schedulepart_tp
R 7516 5 126 typh_types_mod new_mpi_tp v3_schedulepart_tp
R 7517 5 127 typh_types_mod quantsize v3_schedulepart_tp
R 7518 5 128 typh_types_mod nrepeat v3_schedulepart_tp
R 7519 5 129 typh_types_mod stride v3_schedulepart_tp
R 7520 5 130 typh_types_mod key v3_schedulepart_tp
R 7522 5 132 typh_types_mod key$p v3_schedulepart_tp
R 7524 5 134 typh_types_mod old_mpi_tp v3_schedulepart_tp
R 7526 5 136 typh_types_mod old_mpi_tp$p v3_schedulepart_tp
R 7528 5 138 typh_types_mod address v3_schedulepart_tp
R 7530 5 140 typh_types_mod address$p v3_schedulepart_tp
R 7533 25 143 typh_types_mod v3_schedule_tp
R 7534 5 144 typh_types_mod nsend v3_schedule_tp
R 7536 5 146 typh_types_mod send_proc v3_schedule_tp
R 7537 5 147 typh_types_mod send_proc$sd v3_schedule_tp
R 7538 5 148 typh_types_mod send_proc$p v3_schedule_tp
R 7539 5 149 typh_types_mod send_proc$o v3_schedule_tp
R 7542 5 152 typh_types_mod mpi_send_tp v3_schedule_tp
R 7543 5 153 typh_types_mod mpi_send_tp$sd v3_schedule_tp
R 7544 5 154 typh_types_mod mpi_send_tp$p v3_schedule_tp
R 7545 5 155 typh_types_mod mpi_send_tp$o v3_schedule_tp
R 7548 5 158 typh_types_mod send_requests v3_schedule_tp
R 7549 5 159 typh_types_mod send_requests$sd v3_schedule_tp
R 7550 5 160 typh_types_mod send_requests$p v3_schedule_tp
R 7551 5 161 typh_types_mod send_requests$o v3_schedule_tp
R 7554 5 164 typh_types_mod send_nparts v3_schedule_tp
R 7555 5 165 typh_types_mod send_nparts$sd v3_schedule_tp
R 7556 5 166 typh_types_mod send_nparts$p v3_schedule_tp
R 7557 5 167 typh_types_mod send_nparts$o v3_schedule_tp
R 7560 5 170 typh_types_mod send_start v3_schedule_tp
R 7561 5 171 typh_types_mod send_start$sd v3_schedule_tp
R 7562 5 172 typh_types_mod send_start$p v3_schedule_tp
R 7563 5 173 typh_types_mod send_start$o v3_schedule_tp
R 7565 5 175 typh_types_mod nrecv v3_schedule_tp
R 7567 5 177 typh_types_mod recv_proc v3_schedule_tp
R 7568 5 178 typh_types_mod recv_proc$sd v3_schedule_tp
R 7569 5 179 typh_types_mod recv_proc$p v3_schedule_tp
R 7570 5 180 typh_types_mod recv_proc$o v3_schedule_tp
R 7573 5 183 typh_types_mod mpi_recv_tp v3_schedule_tp
R 7574 5 184 typh_types_mod mpi_recv_tp$sd v3_schedule_tp
R 7575 5 185 typh_types_mod mpi_recv_tp$p v3_schedule_tp
R 7576 5 186 typh_types_mod mpi_recv_tp$o v3_schedule_tp
R 7579 5 189 typh_types_mod recv_requests v3_schedule_tp
R 7580 5 190 typh_types_mod recv_requests$sd v3_schedule_tp
R 7581 5 191 typh_types_mod recv_requests$p v3_schedule_tp
R 7582 5 192 typh_types_mod recv_requests$o v3_schedule_tp
R 7585 5 195 typh_types_mod recv_nparts v3_schedule_tp
R 7586 5 196 typh_types_mod recv_nparts$sd v3_schedule_tp
R 7587 5 197 typh_types_mod recv_nparts$p v3_schedule_tp
R 7588 5 198 typh_types_mod recv_nparts$o v3_schedule_tp
R 7591 5 201 typh_types_mod recv_start v3_schedule_tp
R 7592 5 202 typh_types_mod recv_start$sd v3_schedule_tp
R 7593 5 203 typh_types_mod recv_start$p v3_schedule_tp
R 7594 5 204 typh_types_mod recv_start$o v3_schedule_tp
R 7597 5 207 typh_types_mod parts v3_schedule_tp
R 7598 5 208 typh_types_mod parts$sd v3_schedule_tp
R 7599 5 209 typh_types_mod parts$p v3_schedule_tp
R 7600 5 210 typh_types_mod parts$o v3_schedule_tp
R 7609 25 6 typh_util_mod mptypes_tp
R 7610 5 7 typh_util_mod real mptypes_tp
R 7611 5 8 typh_util_mod integer mptypes_tp
R 7612 5 9 typh_util_mod logical mptypes_tp
R 7613 5 10 typh_util_mod character mptypes_tp
R 7614 5 11 typh_util_mod mem mptypes_tp
R 7615 5 12 typh_util_mod size mptypes_tp
R 7616 5 13 typh_util_mod mpi mptypes_tp
R 7617 5 14 typh_util_mod integerpad mptypes_tp
R 7619 25 16 typh_util_mod mp_tp
R 7620 5 17 typh_util_mod comm mp_tp
R 7621 5 18 typh_util_mod info mp_tp
R 7622 5 19 typh_util_mod size mp_tp
R 7623 5 20 typh_util_mod rank mp_tp
R 7624 5 21 typh_util_mod minrank mp_tp
R 7625 5 22 typh_util_mod maxrank mp_tp
R 7626 5 23 typh_util_mod masterrank mp_tp
R 7627 5 24 typh_util_mod error mp_tp
R 7628 5 25 typh_util_mod ismaster mp_tp
R 7629 5 26 typh_util_mod initialised mp_tp
R 7630 5 27 typh_util_mod finalised mp_tp
R 7631 5 28 typh_util_mod logicalpad mp_tp
R 7780 25 16 typh_register_mod phase_tp
R 7781 25 17 typh_register_mod quant_tp
R 7787 5 23 typh_register_mod nghosts phase_tp
R 7788 5 24 typh_register_mod nquants phase_tp
R 7789 5 25 typh_register_mod pure phase_tp
R 7790 5 26 typh_register_mod name phase_tp
R 7792 5 28 typh_register_mod quantlist phase_tp
R 7793 5 29 typh_register_mod quantlist$sd phase_tp
R 7794 5 30 typh_register_mod quantlist$p phase_tp
R 7795 5 31 typh_register_mod quantlist$o phase_tp
R 7797 5 33 typh_register_mod keysetid phase_tp
R 7798 5 34 typh_register_mod ghostsmin phase_tp
R 7799 5 35 typh_register_mod ghostsmax phase_tp
R 7800 5 36 typh_register_mod pqinfo phase_tp
R 7802 5 38 typh_register_mod pqinfo$p phase_tp
R 7804 5 40 typh_register_mod schedule phase_tp
R 7806 5 42 typh_register_mod schedule$p phase_tp
R 7808 5 44 typh_register_mod isbuilt phase_tp
R 7809 5 45 typh_register_mod iscommit phase_tp
R 7812 5 48 typh_register_mod qdataid quant_tp
R 7813 5 49 typh_register_mod nghosts quant_tp
R 7814 5 50 typh_register_mod centring quant_tp
R 7815 5 51 typh_register_mod datatype quant_tp
R 7816 5 52 typh_register_mod pure quant_tp
R 7817 5 53 typh_register_mod aux quant_tp
R 7818 5 54 typh_register_mod name quant_tp
R 7819 5 55 typh_register_mod mpi_datatype quant_tp
R 7820 5 56 typh_register_mod auxid quant_tp
R 7821 5 57 typh_register_mod rank quant_tp
R 7823 5 59 typh_register_mod dims quant_tp
R 7824 5 60 typh_register_mod dims$sd quant_tp
R 7825 5 61 typh_register_mod dims$p quant_tp
R 7826 5 62 typh_register_mod dims$o quant_tp
R 7828 5 64 typh_register_mod meshdim quant_tp
R 7829 5 65 typh_register_mod stride quant_tp
R 7830 5 66 typh_register_mod quant_address quant_tp
R 7832 5 68 typh_register_mod lowbnd quant_tp
R 7833 5 69 typh_register_mod lowbnd$sd quant_tp
R 7834 5 70 typh_register_mod lowbnd$p quant_tp
R 7835 5 71 typh_register_mod lowbnd$o quant_tp
R 7838 5 74 typh_register_mod uppbnd quant_tp
R 7839 5 75 typh_register_mod uppbnd$sd quant_tp
R 7840 5 76 typh_register_mod uppbnd$p quant_tp
R 7841 5 77 typh_register_mod uppbnd$o quant_tp
R 7845 25 81 typh_register_mod pqll_tp
R 7846 5 82 typh_register_mod phase pqll_tp
R 7847 5 83 typh_register_mod quant pqll_tp
R 7849 5 85 typh_register_mod list pqll_tp
R 7850 5 86 typh_register_mod list$sd pqll_tp
R 7851 5 87 typh_register_mod list$p pqll_tp
R 7852 5 88 typh_register_mod list$o pqll_tp
R 7854 5 90 typh_register_mod nlist pqll_tp
R 7855 5 91 typh_register_mod next pqll_tp
R 7857 5 93 typh_register_mod next$p pqll_tp
R 8010 25 1 typh_decomposition_mod part_info_tp
R 8011 5 2 typh_decomposition_mod id part_info_tp
R 8012 5 3 typh_decomposition_mod nlayers part_info_tp
R 8013 5 4 typh_decomposition_mod nodesperelem part_info_tp
R 8015 5 6 typh_decomposition_mod nel_tot part_info_tp
R 8016 5 7 typh_decomposition_mod nel_tot$sd part_info_tp
R 8017 5 8 typh_decomposition_mod nel_tot$p part_info_tp
R 8018 5 9 typh_decomposition_mod nel_tot$o part_info_tp
R 8021 5 12 typh_decomposition_mod nnod_tot part_info_tp
R 8022 5 13 typh_decomposition_mod nnod_tot$sd part_info_tp
R 8023 5 14 typh_decomposition_mod nnod_tot$p part_info_tp
R 8024 5 15 typh_decomposition_mod nnod_tot$o part_info_tp
R 8028 5 19 typh_decomposition_mod el_to_proc part_info_tp
R 8029 5 20 typh_decomposition_mod el_to_proc$sd part_info_tp
R 8030 5 21 typh_decomposition_mod el_to_proc$p part_info_tp
R 8031 5 22 typh_decomposition_mod el_to_proc$o part_info_tp
R 8035 5 26 typh_decomposition_mod nod_to_proc part_info_tp
R 8036 5 27 typh_decomposition_mod nod_to_proc$sd part_info_tp
R 8037 5 28 typh_decomposition_mod nod_to_proc$p part_info_tp
R 8038 5 29 typh_decomposition_mod nod_to_proc$o part_info_tp
R 8041 5 32 typh_decomposition_mod el_loc_to_glob part_info_tp
R 8042 5 33 typh_decomposition_mod el_loc_to_glob$sd part_info_tp
R 8043 5 34 typh_decomposition_mod el_loc_to_glob$p part_info_tp
R 8044 5 35 typh_decomposition_mod el_loc_to_glob$o part_info_tp
R 8047 5 38 typh_decomposition_mod nod_loc_to_glob part_info_tp
R 8048 5 39 typh_decomposition_mod nod_loc_to_glob$sd part_info_tp
R 8049 5 40 typh_decomposition_mod nod_loc_to_glob$p part_info_tp
R 8050 5 41 typh_decomposition_mod nod_loc_to_glob$o part_info_tp
R 8054 5 45 typh_decomposition_mod connectivity part_info_tp
R 8055 5 46 typh_decomposition_mod connectivity$sd part_info_tp
R 8056 5 47 typh_decomposition_mod connectivity$p part_info_tp
R 8057 5 48 typh_decomposition_mod connectivity$o part_info_tp
R 8059 5 50 typh_decomposition_mod name part_info_tp
R 8061 25 52 typh_decomposition_mod partll_tp
R 8062 5 53 typh_decomposition_mod partition partll_tp
R 8063 5 54 typh_decomposition_mod next partll_tp
R 8065 5 56 typh_decomposition_mod next$p partll_tp
R 8280 25 7 typh_key_mod key_set_array_tp
R 8281 5 8 typh_key_mod keyset key_set_array_tp
R 8283 5 10 typh_key_mod keyset$p key_set_array_tp
A 13 2 0 0 0 6 633 0 0 0 13 0 0 0 0 0 0 0 0 0 0
A 15 2 0 0 0 6 634 0 0 0 15 0 0 0 0 0 0 0 0 0 0
A 17 2 0 0 0 6 635 0 0 0 17 0 0 0 0 0 0 0 0 0 0
A 67 1 0 0 0 56 687 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 70 1 0 0 0 65 689 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 77 2 0 0 0 6 721 0 0 0 77 0 0 0 0 0 0 0 0 0 0
A 79 2 0 0 0 6 722 0 0 0 79 0 0 0 0 0 0 0 0 0 0
A 111 2 0 0 0 6 726 0 0 0 111 0 0 0 0 0 0 0 0 0 0
A 117 1 0 1 0 74 748 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 123 1 0 1 0 80 750 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 128 1 0 3 0 86 752 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 152 2 0 0 0 16 766 0 0 0 152 0 0 0 0 0 0 0 0 0 0
A 172 2 0 0 0 6 770 0 0 0 172 0 0 0 0 0 0 0 0 0 0
A 207 2 0 0 0 6 780 0 0 0 207 0 0 0 0 0 0 0 0 0 0
A 1217 2 0 0 811 7 7373 0 0 0 1217 0 0 0 0 0 0 0 0 0 0
Z
J 149 1 1
V 67 56 7 0
S 0 56 0 0 0
A 0 6 0 0 1 2 0
J 150 1 1
V 70 65 7 0
S 0 65 0 0 0
A 0 6 0 0 1 2 0
J 69 1 1
V 117 74 7 0
R 0 77 0 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 15 1
A 0 6 0 0 1 13 1
A 0 6 0 0 1 17 0
J 71 1 1
V 123 80 7 0
R 0 83 0 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 15 1
A 0 6 0 0 1 13 1
A 0 6 0 0 1 17 0
J 73 1 1
V 128 86 7 0
R 0 89 0 0
A 0 6 0 0 1 13 1
A 0 6 0 0 1 17 1
A 0 6 0 0 1 77 0
T 7442 2537 0 3 0 0
A 7448 7 2564 0 1 2 1
A 7449 7 0 0 1 2 1
A 7447 6 0 172 1 2 1
A 7455 7 2566 0 1 2 1
A 7456 7 0 0 1 2 1
A 7454 6 0 172 1 2 1
A 7460 7 2568 0 1 2 1
A 7464 7 2570 0 1 2 1
A 7468 7 2572 0 1 2 0
T 7471 2577 0 3 0 0
A 7482 7 2589 0 1 2 1
A 7486 7 2591 0 1 2 0
T 7489 2596 0 3 0 0
A 7500 7 2614 0 1 2 1
A 7504 7 2616 0 1 2 1
A 7508 7 2618 0 1 2 1
A 7512 7 2620 0 1 2 0
T 7515 2625 0 3 0 0
A 7522 7 2640 0 1 2 1
A 7526 7 2642 0 1 2 1
A 7530 7 2644 0 1 2 0
T 7533 2649 0 3 0 0
A 7550 7 2721 0 1 2 1
A 7551 7 0 0 1 2 1
A 7549 6 0 172 1 2 1
A 7581 7 2723 0 1 2 1
A 7582 7 0 0 1 2 1
A 7580 6 0 172 1 2 1
A 7599 7 2725 0 1 2 1
A 7600 7 0 0 1 2 1
A 7598 6 0 172 1 2 0
T 7609 2824 0 3 0 0
A 7610 6 0 0 1 2 1
A 7611 6 0 0 1 2 1
A 7612 6 0 0 1 2 1
A 7613 6 0 0 1 2 1
A 7614 6 0 0 1 2 1
A 7615 6 0 0 1 2 1
A 7616 6 0 0 1 2 0
T 7619 2833 0 3 0 0
A 7620 6 0 0 1 15 1
A 7621 6 0 0 1 2 1
A 7622 6 0 0 1 2 1
A 7623 6 0 0 1 79 1
A 7624 6 0 0 1 79 1
A 7625 6 0 0 1 79 1
A 7626 6 0 0 1 79 1
A 7627 6 0 0 1 2 1
A 7628 16 0 0 1 152 1
A 7629 16 0 0 1 152 1
A 7630 16 0 0 1 152 0
T 7780 3001 0 3 0 0
A 7802 7 3021 0 1 2 1
A 7806 7 3023 0 1 2 0
T 7781 3028 0 3 0 0
A 7825 7 3052 0 1 2 1
A 7826 7 0 0 1 2 1
A 7824 6 0 172 1 2 1
A 7830 7 0 0 1 1217 0
T 7845 3057 0 3 0 0
T 7846 3001 0 3 0 1
A 7802 7 3021 0 1 2 1
A 7806 7 3023 0 1 2 0
T 7847 3028 0 3 0 1
A 7825 7 3052 0 1 2 1
A 7826 7 0 0 1 2 1
A 7824 6 0 172 1 2 1
A 7830 7 0 0 1 1217 0
A 7851 7 3072 0 1 2 1
A 7852 7 0 0 1 2 1
A 7850 6 0 172 1 2 1
A 7857 7 3074 0 1 2 0
T 8010 3245 0 3 0 0
A 8017 7 3295 0 1 2 1
A 8018 7 0 0 1 2 1
A 8016 6 0 172 1 2 1
A 8023 7 3297 0 1 2 1
A 8024 7 0 0 1 2 1
A 8022 6 0 172 1 2 1
A 8030 7 3299 0 1 2 1
A 8031 7 0 0 1 2 1
A 8029 6 0 207 1 2 1
A 8037 7 3301 0 1 2 1
A 8038 7 0 0 1 2 1
A 8036 6 0 207 1 2 1
A 8043 7 3303 0 1 2 1
A 8044 7 0 0 1 2 1
A 8042 6 0 172 1 2 1
A 8049 7 3305 0 1 2 1
A 8050 7 0 0 1 2 1
A 8048 6 0 172 1 2 1
A 8056 7 3307 0 1 2 1
A 8057 7 0 0 1 2 1
A 8055 6 0 207 1 2 0
T 8061 3312 0 3 0 0
T 8062 3245 0 3 0 0
A 8017 7 3295 0 1 2 1
A 8018 7 0 0 1 2 1
A 8016 6 0 172 1 2 1
A 8023 7 3297 0 1 2 1
A 8024 7 0 0 1 2 1
A 8022 6 0 172 1 2 1
A 8030 7 3299 0 1 2 1
A 8031 7 0 0 1 2 1
A 8029 6 0 207 1 2 1
A 8037 7 3301 0 1 2 1
A 8038 7 0 0 1 2 1
A 8036 6 0 207 1 2 1
A 8043 7 3303 0 1 2 1
A 8044 7 0 0 1 2 1
A 8042 6 0 172 1 2 1
A 8049 7 3305 0 1 2 1
A 8050 7 0 0 1 2 1
A 8048 6 0 172 1 2 1
A 8056 7 3307 0 1 2 1
A 8057 7 0 0 1 2 1
A 8055 6 0 207 1 2 0
T 8280 3659 0 3 0 0
A 8283 7 3668 0 1 2 0
Z

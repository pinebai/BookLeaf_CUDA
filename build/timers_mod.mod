V30 :0x4 timers_mod
23 ../src/utils/timers.f90 S624 0
05/11/2017  17:52:33
use typh_types_mod private
use iso_fortran_env private
use iso_c_binding private
use typh_util_mod private
use timing_mod private
use kinds_mod private
enduse
D 65 24 695 8 694 7
D 74 24 698 8 697 7
D 83 21 6 1 3 13 0 0 0 0 0
 0 13 3 3 13 13
D 86 21 6 1 3 13 0 0 0 0 0
 0 13 3 3 13 13
D 89 21 6 1 3 13 0 0 0 0 0
 0 13 3 3 13 13
D 92 21 6 1 3 13 0 0 0 0 0
 0 13 3 3 13 13
D 95 21 6 1 3 114 0 0 0 0 0
 0 114 3 3 114 114
D 98 21 6 1 3 114 0 0 0 0 0
 0 114 3 3 114 114
D 2546 24 7489 216 7488 7
D 2573 20 6
D 2575 20 6
D 2577 20 2546
D 2579 20 2546
D 2581 20 2546
D 2586 24 7518 48 7517 7
D 2598 20 2546
D 2600 20 2546
D 2605 24 7536 64 7535 7
D 2623 20 6
D 2625 20 6
D 2627 20 2586
D 2629 20 2605
D 2634 24 7562 40 7561 7
D 2649 20 2546
D 2651 20 6
D 2653 20 7
D 2658 24 7580 984 7579 7
D 2730 20 6
D 2732 20 6
D 2734 20 2634
D 2833 24 7656 32 7655 3
D 2842 24 7666 48 7665 3
S 624 24 0 0 0 8 1 0 5011 10005 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 19 0 0 0 0 0 0 timers_mod
S 626 23 0 0 0 8 634 624 5032 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rlk
S 628 23 0 0 0 8 677 624 5047 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 bookleaf_times
S 630 23 0 0 0 8 7791 624 5076 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 get_time
S 631 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 632 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 634 16 2 kinds_mod rlk
R 677 6 42 timing_mod bookleaf_times
S 680 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 694 25 6 iso_c_binding c_ptr
R 695 5 7 iso_c_binding val c_ptr
R 697 25 9 iso_c_binding c_funptr
R 698 5 10 iso_c_binding val c_funptr
R 732 6 44 iso_c_binding c_null_ptr$ac
R 734 6 46 iso_c_binding c_null_funptr$ac
R 735 26 47 iso_c_binding ==
R 737 26 49 iso_c_binding !=
S 766 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 767 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 771 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 793 7 22 iso_fortran_env integer_kinds$ac
R 795 7 24 iso_fortran_env logical_kinds$ac
R 797 7 26 iso_fortran_env real_kinds$ac
S 811 3 0 0 0 16 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16
S 815 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 7488 25 52 typh_types_mod keyll_tp
R 7489 5 53 typh_types_mod layer keyll_tp
R 7490 5 54 typh_types_mod proc keyll_tp
R 7492 5 56 typh_types_mod list keyll_tp
R 7493 5 57 typh_types_mod list$sd keyll_tp
R 7494 5 58 typh_types_mod list$p keyll_tp
R 7495 5 59 typh_types_mod list$o keyll_tp
R 7497 5 61 typh_types_mod nlist keyll_tp
R 7499 5 63 typh_types_mod blocklens keyll_tp
R 7500 5 64 typh_types_mod blocklens$sd keyll_tp
R 7501 5 65 typh_types_mod blocklens$p keyll_tp
R 7502 5 66 typh_types_mod blocklens$o keyll_tp
R 7504 5 68 typh_types_mod next keyll_tp
R 7506 5 70 typh_types_mod next$p keyll_tp
R 7508 5 72 typh_types_mod prev keyll_tp
R 7510 5 74 typh_types_mod prev$p keyll_tp
R 7512 5 76 typh_types_mod parent keyll_tp
R 7514 5 78 typh_types_mod parent$p keyll_tp
R 7517 25 81 typh_types_mod key_set_tp
R 7518 5 82 typh_types_mod centring key_set_tp
R 7519 5 83 typh_types_mod auxid key_set_tp
R 7520 5 84 typh_types_mod stride key_set_tp
R 7521 5 85 typh_types_mod lmin key_set_tp
R 7522 5 86 typh_types_mod lmax key_set_tp
R 7523 5 87 typh_types_mod partitionid key_set_tp
R 7524 5 88 typh_types_mod nsend key_set_tp
R 7525 5 89 typh_types_mod nrecv key_set_tp
R 7526 5 90 typh_types_mod send_keys key_set_tp
R 7528 5 92 typh_types_mod send_keys$p key_set_tp
R 7530 5 94 typh_types_mod recv_keys key_set_tp
R 7532 5 96 typh_types_mod recv_keys$p key_set_tp
R 7535 25 99 typh_types_mod v3_comm_quant_tp
R 7536 5 100 typh_types_mod quantid v3_comm_quant_tp
R 7537 5 101 typh_types_mod receivequantid v3_comm_quant_tp
R 7538 5 102 typh_types_mod keysetid v3_comm_quant_tp
R 7539 5 103 typh_types_mod ghostsmin v3_comm_quant_tp
R 7540 5 104 typh_types_mod ghostsmax v3_comm_quant_tp
R 7541 5 105 typh_types_mod quantsize v3_comm_quant_tp
R 7542 5 106 typh_types_mod nrepeat v3_comm_quant_tp
R 7543 5 107 typh_types_mod stride v3_comm_quant_tp
R 7544 5 108 typh_types_mod oldmpitype v3_comm_quant_tp
R 7546 5 110 typh_types_mod oldmpitype$p v3_comm_quant_tp
R 7548 5 112 typh_types_mod newmpitype v3_comm_quant_tp
R 7550 5 114 typh_types_mod newmpitype$p v3_comm_quant_tp
R 7552 5 116 typh_types_mod keyset v3_comm_quant_tp
R 7554 5 118 typh_types_mod keyset$p v3_comm_quant_tp
R 7556 5 120 typh_types_mod next v3_comm_quant_tp
R 7558 5 122 typh_types_mod next$p v3_comm_quant_tp
R 7561 25 125 typh_types_mod v3_schedulepart_tp
R 7562 5 126 typh_types_mod new_mpi_tp v3_schedulepart_tp
R 7563 5 127 typh_types_mod quantsize v3_schedulepart_tp
R 7564 5 128 typh_types_mod nrepeat v3_schedulepart_tp
R 7565 5 129 typh_types_mod stride v3_schedulepart_tp
R 7566 5 130 typh_types_mod key v3_schedulepart_tp
R 7568 5 132 typh_types_mod key$p v3_schedulepart_tp
R 7570 5 134 typh_types_mod old_mpi_tp v3_schedulepart_tp
R 7572 5 136 typh_types_mod old_mpi_tp$p v3_schedulepart_tp
R 7574 5 138 typh_types_mod address v3_schedulepart_tp
R 7576 5 140 typh_types_mod address$p v3_schedulepart_tp
R 7579 25 143 typh_types_mod v3_schedule_tp
R 7580 5 144 typh_types_mod nsend v3_schedule_tp
R 7582 5 146 typh_types_mod send_proc v3_schedule_tp
R 7583 5 147 typh_types_mod send_proc$sd v3_schedule_tp
R 7584 5 148 typh_types_mod send_proc$p v3_schedule_tp
R 7585 5 149 typh_types_mod send_proc$o v3_schedule_tp
R 7588 5 152 typh_types_mod mpi_send_tp v3_schedule_tp
R 7589 5 153 typh_types_mod mpi_send_tp$sd v3_schedule_tp
R 7590 5 154 typh_types_mod mpi_send_tp$p v3_schedule_tp
R 7591 5 155 typh_types_mod mpi_send_tp$o v3_schedule_tp
R 7594 5 158 typh_types_mod send_requests v3_schedule_tp
R 7595 5 159 typh_types_mod send_requests$sd v3_schedule_tp
R 7596 5 160 typh_types_mod send_requests$p v3_schedule_tp
R 7597 5 161 typh_types_mod send_requests$o v3_schedule_tp
R 7600 5 164 typh_types_mod send_nparts v3_schedule_tp
R 7601 5 165 typh_types_mod send_nparts$sd v3_schedule_tp
R 7602 5 166 typh_types_mod send_nparts$p v3_schedule_tp
R 7603 5 167 typh_types_mod send_nparts$o v3_schedule_tp
R 7606 5 170 typh_types_mod send_start v3_schedule_tp
R 7607 5 171 typh_types_mod send_start$sd v3_schedule_tp
R 7608 5 172 typh_types_mod send_start$p v3_schedule_tp
R 7609 5 173 typh_types_mod send_start$o v3_schedule_tp
R 7611 5 175 typh_types_mod nrecv v3_schedule_tp
R 7613 5 177 typh_types_mod recv_proc v3_schedule_tp
R 7614 5 178 typh_types_mod recv_proc$sd v3_schedule_tp
R 7615 5 179 typh_types_mod recv_proc$p v3_schedule_tp
R 7616 5 180 typh_types_mod recv_proc$o v3_schedule_tp
R 7619 5 183 typh_types_mod mpi_recv_tp v3_schedule_tp
R 7620 5 184 typh_types_mod mpi_recv_tp$sd v3_schedule_tp
R 7621 5 185 typh_types_mod mpi_recv_tp$p v3_schedule_tp
R 7622 5 186 typh_types_mod mpi_recv_tp$o v3_schedule_tp
R 7625 5 189 typh_types_mod recv_requests v3_schedule_tp
R 7626 5 190 typh_types_mod recv_requests$sd v3_schedule_tp
R 7627 5 191 typh_types_mod recv_requests$p v3_schedule_tp
R 7628 5 192 typh_types_mod recv_requests$o v3_schedule_tp
R 7631 5 195 typh_types_mod recv_nparts v3_schedule_tp
R 7632 5 196 typh_types_mod recv_nparts$sd v3_schedule_tp
R 7633 5 197 typh_types_mod recv_nparts$p v3_schedule_tp
R 7634 5 198 typh_types_mod recv_nparts$o v3_schedule_tp
R 7637 5 201 typh_types_mod recv_start v3_schedule_tp
R 7638 5 202 typh_types_mod recv_start$sd v3_schedule_tp
R 7639 5 203 typh_types_mod recv_start$p v3_schedule_tp
R 7640 5 204 typh_types_mod recv_start$o v3_schedule_tp
R 7643 5 207 typh_types_mod parts v3_schedule_tp
R 7644 5 208 typh_types_mod parts$sd v3_schedule_tp
R 7645 5 209 typh_types_mod parts$p v3_schedule_tp
R 7646 5 210 typh_types_mod parts$o v3_schedule_tp
R 7655 25 6 typh_util_mod mptypes_tp
R 7656 5 7 typh_util_mod real mptypes_tp
R 7657 5 8 typh_util_mod integer mptypes_tp
R 7658 5 9 typh_util_mod logical mptypes_tp
R 7659 5 10 typh_util_mod character mptypes_tp
R 7660 5 11 typh_util_mod mem mptypes_tp
R 7661 5 12 typh_util_mod size mptypes_tp
R 7662 5 13 typh_util_mod mpi mptypes_tp
R 7663 5 14 typh_util_mod integerpad mptypes_tp
R 7665 25 16 typh_util_mod mp_tp
R 7666 5 17 typh_util_mod comm mp_tp
R 7667 5 18 typh_util_mod info mp_tp
R 7668 5 19 typh_util_mod size mp_tp
R 7669 5 20 typh_util_mod rank mp_tp
R 7670 5 21 typh_util_mod minrank mp_tp
R 7671 5 22 typh_util_mod maxrank mp_tp
R 7672 5 23 typh_util_mod masterrank mp_tp
R 7673 5 24 typh_util_mod error mp_tp
R 7674 5 25 typh_util_mod ismaster mp_tp
R 7675 5 26 typh_util_mod initialised mp_tp
R 7676 5 27 typh_util_mod finalised mp_tp
R 7677 5 28 typh_util_mod logicalpad mp_tp
R 7791 14 142 typh_util_mod get_time
S 7805 23 5 0 0 0 7806 624 38065 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 start_timers
S 7806 14 5 0 0 0 1 7805 38065 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 4936 0 0 0 0 0 0 0 0 0 0 0 0 0 29 0 624 0 0 0 0 start_timers
F 7806 0
S 7807 23 5 0 0 0 7808 624 38078 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 end_timers
S 7808 14 5 0 0 0 1 7807 38078 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 4937 0 0 0 0 0 0 0 0 0 0 0 0 0 72 0 624 0 0 0 0 end_timers
F 7808 0
S 7809 23 5 0 0 0 7810 624 38089 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 print_timers
S 7810 14 5 0 0 0 1 7809 38089 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 4938 0 0 0 0 0 0 0 0 0 0 0 0 0 79 0 624 0 0 0 0 print_timers
F 7810 0
A 13 2 0 0 0 6 631 0 0 0 13 0 0 0 0 0 0 0 0 0 0
A 15 2 0 0 0 6 632 0 0 0 15 0 0 0 0 0 0 0 0 0 0
A 19 2 0 0 0 6 680 0 0 0 19 0 0 0 0 0 0 0 0 0 0
A 70 1 0 0 0 65 732 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 73 1 0 0 0 74 734 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 80 2 0 0 0 6 766 0 0 0 80 0 0 0 0 0 0 0 0 0 0
A 82 2 0 0 0 6 767 0 0 0 82 0 0 0 0 0 0 0 0 0 0
A 114 2 0 0 0 6 771 0 0 0 114 0 0 0 0 0 0 0 0 0 0
A 120 1 0 1 0 83 793 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 126 1 0 1 0 89 795 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 131 1 0 3 0 95 797 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 155 2 0 0 0 16 811 0 0 0 155 0 0 0 0 0 0 0 0 0 0
A 175 2 0 0 0 6 815 0 0 0 175 0 0 0 0 0 0 0 0 0 0
Z
J 149 1 1
V 70 65 7 0
S 0 65 0 0 0
A 0 6 0 0 1 2 0
J 150 1 1
V 73 74 7 0
S 0 74 0 0 0
A 0 6 0 0 1 2 0
J 69 1 1
V 120 83 7 0
R 0 86 0 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 19 1
A 0 6 0 0 1 13 1
A 0 6 0 0 1 15 0
J 71 1 1
V 126 89 7 0
R 0 92 0 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 19 1
A 0 6 0 0 1 13 1
A 0 6 0 0 1 15 0
J 73 1 1
V 131 95 7 0
R 0 98 0 0
A 0 6 0 0 1 13 1
A 0 6 0 0 1 15 1
A 0 6 0 0 1 80 0
T 7488 2546 0 3 0 0
A 7494 7 2573 0 1 2 1
A 7495 7 0 0 1 2 1
A 7493 6 0 175 1 2 1
A 7501 7 2575 0 1 2 1
A 7502 7 0 0 1 2 1
A 7500 6 0 175 1 2 1
A 7506 7 2577 0 1 2 1
A 7510 7 2579 0 1 2 1
A 7514 7 2581 0 1 2 0
T 7517 2586 0 3 0 0
A 7528 7 2598 0 1 2 1
A 7532 7 2600 0 1 2 0
T 7535 2605 0 3 0 0
A 7546 7 2623 0 1 2 1
A 7550 7 2625 0 1 2 1
A 7554 7 2627 0 1 2 1
A 7558 7 2629 0 1 2 0
T 7561 2634 0 3 0 0
A 7568 7 2649 0 1 2 1
A 7572 7 2651 0 1 2 1
A 7576 7 2653 0 1 2 0
T 7579 2658 0 3 0 0
A 7596 7 2730 0 1 2 1
A 7597 7 0 0 1 2 1
A 7595 6 0 175 1 2 1
A 7627 7 2732 0 1 2 1
A 7628 7 0 0 1 2 1
A 7626 6 0 175 1 2 1
A 7645 7 2734 0 1 2 1
A 7646 7 0 0 1 2 1
A 7644 6 0 175 1 2 0
T 7655 2833 0 3 0 0
A 7656 6 0 0 1 2 1
A 7657 6 0 0 1 2 1
A 7658 6 0 0 1 2 1
A 7659 6 0 0 1 2 1
A 7660 6 0 0 1 2 1
A 7661 6 0 0 1 2 1
A 7662 6 0 0 1 2 0
T 7665 2842 0 3 0 0
A 7666 6 0 0 1 19 1
A 7667 6 0 0 1 2 1
A 7668 6 0 0 1 2 1
A 7669 6 0 0 1 82 1
A 7670 6 0 0 1 82 1
A 7671 6 0 0 1 82 1
A 7672 6 0 0 1 82 1
A 7673 6 0 0 1 2 1
A 7674 16 0 0 1 155 1
A 7675 16 0 0 1 155 1
A 7676 16 0 0 1 155 0
Z

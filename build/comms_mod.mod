V30 :0x4 comms_mod
21 ../src/comms/comm.F90 S624 0
05/11/2017  17:52:23
use typh_key_mod private
use typh_register_mod private
use typh_decomposition_mod private
use typh_util_mod private
use typh_types_mod private
use iso_fortran_env private
use iso_c_binding private
use kinds_mod private
enduse
D 56 24 646 8 645 7
D 65 24 649 8 648 7
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
D 2537 24 7440 216 7439 7
D 2564 20 6
D 2566 20 6
D 2568 20 2537
D 2570 20 2537
D 2572 20 2537
D 2577 24 7469 48 7468 7
D 2589 20 2537
D 2591 20 2537
D 2596 24 7487 64 7486 7
D 2614 20 6
D 2616 20 6
D 2618 20 2577
D 2620 20 2596
D 2625 24 7513 40 7512 7
D 2640 20 2537
D 2642 20 6
D 2644 20 7
D 2649 24 7531 984 7530 7
D 2721 20 6
D 2723 20 6
D 2725 20 2625
D 2824 24 7608 32 7607 3
D 2833 24 7618 48 7617 3
D 3001 24 7759 768 7758 7
D 3051 20 6
D 3053 20 6
D 3055 20 6
D 3057 20 6
D 3059 20 6
D 3061 20 6
D 3063 20 6
D 3068 24 7810 776 7809 7
D 3178 24 7487 64 7486 7
D 3204 24 7531 984 7530 7
D 3228 24 7931 208 7924 7
D 3248 20 3178
D 3250 20 3204
D 3255 24 7956 384 7925 7
D 3279 20 6
D 3284 24 7990 696 7989 7
D 3299 20 6
D 3301 20 3284
D 3533 24 7469 48 7468 7
D 3659 24 8282 8 8281 7
D 3668 20 3533
D 4485 21 6 1 1716 1715 0 1 0 0 1
 1710 1713 1714 1710 1713 1711
D 4488 21 6 1 0 172 0 0 0 0 0
 0 172 0 3 172 0
D 4491 21 6 1 1725 1724 0 1 0 0 1
 1719 1722 1723 1719 1722 1720
D 4494 21 6 1 0 172 0 0 0 0 0
 0 172 0 3 172 0
D 4497 21 6 2 1727 1732 0 0 1 0 0
 0 1728 3 3 1729 1729
 0 1730 1729 3 1731 1731
D 4500 21 6 2 1733 1738 0 0 1 0 0
 0 1734 3 3 1735 1735
 0 1736 1735 3 1737 1737
D 4503 21 6 1 1739 1742 1 1 0 0 1
 3 1740 3 3 1740 1741
D 4506 21 6 1 1743 1746 1 1 0 0 1
 3 1744 3 3 1744 1745
D 4509 21 6 1 1748 1754 0 1 0 0 1
 1749 1752 1753 1749 1752 1750
D 4512 21 6 1 0 172 0 0 0 0 0
 0 172 0 3 172 0
D 4515 21 6 1 1756 1762 0 1 0 0 1
 1757 1760 1761 1757 1760 1758
D 4518 21 6 1 0 172 0 0 0 0 0
 0 172 0 3 172 0
D 4521 21 6 2 1764 1775 0 1 0 0 1
 1765 1768 1769 1765 1768 1766
 1770 1773 1774 1770 1773 1771
D 4524 21 6 1 0 207 0 0 0 0 0
 0 207 0 3 207 0
D 4527 21 6 2 1777 1788 0 1 0 0 1
 1778 1781 1782 1778 1781 1779
 1783 1786 1787 1783 1786 1784
D 4530 21 6 1 0 207 0 0 0 0 0
 0 207 0 3 207 0
D 4533 21 6 2 1790 1801 0 1 0 0 1
 1791 1794 1795 1791 1794 1792
 1796 1799 1800 1796 1799 1797
D 4536 21 6 1 0 207 0 0 0 0 0
 0 207 0 3 207 0
D 4539 21 6 2 1803 1814 0 1 0 0 1
 1804 1807 1808 1804 1807 1805
 1809 1812 1813 1809 1812 1810
D 4542 21 6 1 0 207 0 0 0 0 0
 0 207 0 3 207 0
D 4545 21 6 1 1816 1822 0 1 0 0 1
 1817 1820 1821 1817 1820 1818
D 4548 21 6 1 0 172 0 0 0 0 0
 0 172 0 3 172 0
D 4551 21 16 2 1823 1830 1 1 0 0 1
 2 1824 3 1825 1824 1826
 3 1827 1828 3 1827 1829
D 4554 21 16 2 1831 1838 1 1 0 0 1
 2 1832 3 1833 1832 1834
 3 1835 1836 3 1835 1837
D 4557 21 6 2 1839 1846 1 1 0 0 1
 2 1840 3 1841 1840 1842
 3 1843 1844 3 1843 1845
D 4560 21 6 2 1847 1854 1 1 0 0 1
 2 1848 3 1849 1848 1850
 3 1851 1852 3 1851 1853
D 4563 21 6 2 1855 1861 1 1 0 0 1
 3 1856 3 3 1856 1857
 3 1858 1859 3 1858 1860
D 4566 21 6 2 1862 1868 1 1 0 0 1
 3 1863 3 3 1863 1864
 3 1865 1866 3 1865 1867
D 4569 21 6 1 1869 1873 1 1 0 0 1
 2 1870 3 1871 1870 1872
D 4572 21 6 1 1874 1877 1 1 0 0 1
 3 1875 3 3 1875 1876
D 4575 21 16 1 1878 1881 1 1 0 0 1
 3 1879 3 3 1879 1880
S 624 24 0 0 0 8 1 0 5011 10015 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 19 0 0 0 0 0 0 comms_mod
S 627 23 0 0 0 6 8487 624 5038 14 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ink
S 629 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 630 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 631 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 645 25 6 iso_c_binding c_ptr
R 646 5 7 iso_c_binding val c_ptr
R 648 25 9 iso_c_binding c_funptr
R 649 5 10 iso_c_binding val c_funptr
R 683 6 44 iso_c_binding c_null_ptr$ac
R 685 6 46 iso_c_binding c_null_funptr$ac
R 686 26 47 iso_c_binding ==
R 688 26 49 iso_c_binding !=
S 717 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 718 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 722 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 744 7 22 iso_fortran_env integer_kinds$ac
R 746 7 24 iso_fortran_env logical_kinds$ac
R 748 7 26 iso_fortran_env real_kinds$ac
S 751 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 762 3 0 0 0 16 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16
S 765 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 766 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 767 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 769 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 772 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 775 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 23 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 776 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 793 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 19 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 7370 3 0 0 0 7 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -999 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7
R 7439 25 52 typh_types_mod keyll_tp
R 7440 5 53 typh_types_mod layer keyll_tp
R 7441 5 54 typh_types_mod proc keyll_tp
R 7443 5 56 typh_types_mod list keyll_tp
R 7444 5 57 typh_types_mod list$sd keyll_tp
R 7445 5 58 typh_types_mod list$p keyll_tp
R 7446 5 59 typh_types_mod list$o keyll_tp
R 7448 5 61 typh_types_mod nlist keyll_tp
R 7450 5 63 typh_types_mod blocklens keyll_tp
R 7451 5 64 typh_types_mod blocklens$sd keyll_tp
R 7452 5 65 typh_types_mod blocklens$p keyll_tp
R 7453 5 66 typh_types_mod blocklens$o keyll_tp
R 7455 5 68 typh_types_mod next keyll_tp
R 7457 5 70 typh_types_mod next$p keyll_tp
R 7459 5 72 typh_types_mod prev keyll_tp
R 7461 5 74 typh_types_mod prev$p keyll_tp
R 7463 5 76 typh_types_mod parent keyll_tp
R 7465 5 78 typh_types_mod parent$p keyll_tp
R 7468 25 81 typh_types_mod key_set_tp
R 7469 5 82 typh_types_mod centring key_set_tp
R 7470 5 83 typh_types_mod auxid key_set_tp
R 7471 5 84 typh_types_mod stride key_set_tp
R 7472 5 85 typh_types_mod lmin key_set_tp
R 7473 5 86 typh_types_mod lmax key_set_tp
R 7474 5 87 typh_types_mod partitionid key_set_tp
R 7475 5 88 typh_types_mod nsend key_set_tp
R 7476 5 89 typh_types_mod nrecv key_set_tp
R 7477 5 90 typh_types_mod send_keys key_set_tp
R 7479 5 92 typh_types_mod send_keys$p key_set_tp
R 7481 5 94 typh_types_mod recv_keys key_set_tp
R 7483 5 96 typh_types_mod recv_keys$p key_set_tp
R 7486 25 99 typh_types_mod v3_comm_quant_tp
R 7487 5 100 typh_types_mod quantid v3_comm_quant_tp
R 7488 5 101 typh_types_mod receivequantid v3_comm_quant_tp
R 7489 5 102 typh_types_mod keysetid v3_comm_quant_tp
R 7490 5 103 typh_types_mod ghostsmin v3_comm_quant_tp
R 7491 5 104 typh_types_mod ghostsmax v3_comm_quant_tp
R 7492 5 105 typh_types_mod quantsize v3_comm_quant_tp
R 7493 5 106 typh_types_mod nrepeat v3_comm_quant_tp
R 7494 5 107 typh_types_mod stride v3_comm_quant_tp
R 7495 5 108 typh_types_mod oldmpitype v3_comm_quant_tp
R 7497 5 110 typh_types_mod oldmpitype$p v3_comm_quant_tp
R 7499 5 112 typh_types_mod newmpitype v3_comm_quant_tp
R 7501 5 114 typh_types_mod newmpitype$p v3_comm_quant_tp
R 7503 5 116 typh_types_mod keyset v3_comm_quant_tp
R 7505 5 118 typh_types_mod keyset$p v3_comm_quant_tp
R 7507 5 120 typh_types_mod next v3_comm_quant_tp
R 7509 5 122 typh_types_mod next$p v3_comm_quant_tp
R 7512 25 125 typh_types_mod v3_schedulepart_tp
R 7513 5 126 typh_types_mod new_mpi_tp v3_schedulepart_tp
R 7514 5 127 typh_types_mod quantsize v3_schedulepart_tp
R 7515 5 128 typh_types_mod nrepeat v3_schedulepart_tp
R 7516 5 129 typh_types_mod stride v3_schedulepart_tp
R 7517 5 130 typh_types_mod key v3_schedulepart_tp
R 7519 5 132 typh_types_mod key$p v3_schedulepart_tp
R 7521 5 134 typh_types_mod old_mpi_tp v3_schedulepart_tp
R 7523 5 136 typh_types_mod old_mpi_tp$p v3_schedulepart_tp
R 7525 5 138 typh_types_mod address v3_schedulepart_tp
R 7527 5 140 typh_types_mod address$p v3_schedulepart_tp
R 7530 25 143 typh_types_mod v3_schedule_tp
R 7531 5 144 typh_types_mod nsend v3_schedule_tp
R 7533 5 146 typh_types_mod send_proc v3_schedule_tp
R 7534 5 147 typh_types_mod send_proc$sd v3_schedule_tp
R 7535 5 148 typh_types_mod send_proc$p v3_schedule_tp
R 7536 5 149 typh_types_mod send_proc$o v3_schedule_tp
R 7539 5 152 typh_types_mod mpi_send_tp v3_schedule_tp
R 7540 5 153 typh_types_mod mpi_send_tp$sd v3_schedule_tp
R 7541 5 154 typh_types_mod mpi_send_tp$p v3_schedule_tp
R 7542 5 155 typh_types_mod mpi_send_tp$o v3_schedule_tp
R 7545 5 158 typh_types_mod send_requests v3_schedule_tp
R 7546 5 159 typh_types_mod send_requests$sd v3_schedule_tp
R 7547 5 160 typh_types_mod send_requests$p v3_schedule_tp
R 7548 5 161 typh_types_mod send_requests$o v3_schedule_tp
R 7551 5 164 typh_types_mod send_nparts v3_schedule_tp
R 7552 5 165 typh_types_mod send_nparts$sd v3_schedule_tp
R 7553 5 166 typh_types_mod send_nparts$p v3_schedule_tp
R 7554 5 167 typh_types_mod send_nparts$o v3_schedule_tp
R 7557 5 170 typh_types_mod send_start v3_schedule_tp
R 7558 5 171 typh_types_mod send_start$sd v3_schedule_tp
R 7559 5 172 typh_types_mod send_start$p v3_schedule_tp
R 7560 5 173 typh_types_mod send_start$o v3_schedule_tp
R 7562 5 175 typh_types_mod nrecv v3_schedule_tp
R 7564 5 177 typh_types_mod recv_proc v3_schedule_tp
R 7565 5 178 typh_types_mod recv_proc$sd v3_schedule_tp
R 7566 5 179 typh_types_mod recv_proc$p v3_schedule_tp
R 7567 5 180 typh_types_mod recv_proc$o v3_schedule_tp
R 7570 5 183 typh_types_mod mpi_recv_tp v3_schedule_tp
R 7571 5 184 typh_types_mod mpi_recv_tp$sd v3_schedule_tp
R 7572 5 185 typh_types_mod mpi_recv_tp$p v3_schedule_tp
R 7573 5 186 typh_types_mod mpi_recv_tp$o v3_schedule_tp
R 7576 5 189 typh_types_mod recv_requests v3_schedule_tp
R 7577 5 190 typh_types_mod recv_requests$sd v3_schedule_tp
R 7578 5 191 typh_types_mod recv_requests$p v3_schedule_tp
R 7579 5 192 typh_types_mod recv_requests$o v3_schedule_tp
R 7582 5 195 typh_types_mod recv_nparts v3_schedule_tp
R 7583 5 196 typh_types_mod recv_nparts$sd v3_schedule_tp
R 7584 5 197 typh_types_mod recv_nparts$p v3_schedule_tp
R 7585 5 198 typh_types_mod recv_nparts$o v3_schedule_tp
R 7588 5 201 typh_types_mod recv_start v3_schedule_tp
R 7589 5 202 typh_types_mod recv_start$sd v3_schedule_tp
R 7590 5 203 typh_types_mod recv_start$p v3_schedule_tp
R 7591 5 204 typh_types_mod recv_start$o v3_schedule_tp
R 7594 5 207 typh_types_mod parts v3_schedule_tp
R 7595 5 208 typh_types_mod parts$sd v3_schedule_tp
R 7596 5 209 typh_types_mod parts$p v3_schedule_tp
R 7597 5 210 typh_types_mod parts$o v3_schedule_tp
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
R 7758 25 1 typh_decomposition_mod part_info_tp
R 7759 5 2 typh_decomposition_mod id part_info_tp
R 7760 5 3 typh_decomposition_mod nlayers part_info_tp
R 7761 5 4 typh_decomposition_mod nodesperelem part_info_tp
R 7763 5 6 typh_decomposition_mod nel_tot part_info_tp
R 7764 5 7 typh_decomposition_mod nel_tot$sd part_info_tp
R 7765 5 8 typh_decomposition_mod nel_tot$p part_info_tp
R 7766 5 9 typh_decomposition_mod nel_tot$o part_info_tp
R 7769 5 12 typh_decomposition_mod nnod_tot part_info_tp
R 7770 5 13 typh_decomposition_mod nnod_tot$sd part_info_tp
R 7771 5 14 typh_decomposition_mod nnod_tot$p part_info_tp
R 7772 5 15 typh_decomposition_mod nnod_tot$o part_info_tp
R 7776 5 19 typh_decomposition_mod el_to_proc part_info_tp
R 7777 5 20 typh_decomposition_mod el_to_proc$sd part_info_tp
R 7778 5 21 typh_decomposition_mod el_to_proc$p part_info_tp
R 7779 5 22 typh_decomposition_mod el_to_proc$o part_info_tp
R 7783 5 26 typh_decomposition_mod nod_to_proc part_info_tp
R 7784 5 27 typh_decomposition_mod nod_to_proc$sd part_info_tp
R 7785 5 28 typh_decomposition_mod nod_to_proc$p part_info_tp
R 7786 5 29 typh_decomposition_mod nod_to_proc$o part_info_tp
R 7789 5 32 typh_decomposition_mod el_loc_to_glob part_info_tp
R 7790 5 33 typh_decomposition_mod el_loc_to_glob$sd part_info_tp
R 7791 5 34 typh_decomposition_mod el_loc_to_glob$p part_info_tp
R 7792 5 35 typh_decomposition_mod el_loc_to_glob$o part_info_tp
R 7795 5 38 typh_decomposition_mod nod_loc_to_glob part_info_tp
R 7796 5 39 typh_decomposition_mod nod_loc_to_glob$sd part_info_tp
R 7797 5 40 typh_decomposition_mod nod_loc_to_glob$p part_info_tp
R 7798 5 41 typh_decomposition_mod nod_loc_to_glob$o part_info_tp
R 7802 5 45 typh_decomposition_mod connectivity part_info_tp
R 7803 5 46 typh_decomposition_mod connectivity$sd part_info_tp
R 7804 5 47 typh_decomposition_mod connectivity$p part_info_tp
R 7805 5 48 typh_decomposition_mod connectivity$o part_info_tp
R 7807 5 50 typh_decomposition_mod name part_info_tp
R 7809 25 52 typh_decomposition_mod partll_tp
R 7810 5 53 typh_decomposition_mod partition partll_tp
R 7811 5 54 typh_decomposition_mod next partll_tp
R 7813 5 56 typh_decomposition_mod next$p partll_tp
R 7924 25 16 typh_register_mod phase_tp
R 7925 25 17 typh_register_mod quant_tp
R 7931 5 23 typh_register_mod nghosts phase_tp
R 7932 5 24 typh_register_mod nquants phase_tp
R 7933 5 25 typh_register_mod pure phase_tp
R 7934 5 26 typh_register_mod name phase_tp
R 7936 5 28 typh_register_mod quantlist phase_tp
R 7937 5 29 typh_register_mod quantlist$sd phase_tp
R 7938 5 30 typh_register_mod quantlist$p phase_tp
R 7939 5 31 typh_register_mod quantlist$o phase_tp
R 7941 5 33 typh_register_mod keysetid phase_tp
R 7942 5 34 typh_register_mod ghostsmin phase_tp
R 7943 5 35 typh_register_mod ghostsmax phase_tp
R 7944 5 36 typh_register_mod pqinfo phase_tp
R 7946 5 38 typh_register_mod pqinfo$p phase_tp
R 7948 5 40 typh_register_mod schedule phase_tp
R 7950 5 42 typh_register_mod schedule$p phase_tp
R 7952 5 44 typh_register_mod isbuilt phase_tp
R 7953 5 45 typh_register_mod iscommit phase_tp
R 7956 5 48 typh_register_mod qdataid quant_tp
R 7957 5 49 typh_register_mod nghosts quant_tp
R 7958 5 50 typh_register_mod centring quant_tp
R 7959 5 51 typh_register_mod datatype quant_tp
R 7960 5 52 typh_register_mod pure quant_tp
R 7961 5 53 typh_register_mod aux quant_tp
R 7962 5 54 typh_register_mod name quant_tp
R 7963 5 55 typh_register_mod mpi_datatype quant_tp
R 7964 5 56 typh_register_mod auxid quant_tp
R 7965 5 57 typh_register_mod rank quant_tp
R 7967 5 59 typh_register_mod dims quant_tp
R 7968 5 60 typh_register_mod dims$sd quant_tp
R 7969 5 61 typh_register_mod dims$p quant_tp
R 7970 5 62 typh_register_mod dims$o quant_tp
R 7972 5 64 typh_register_mod meshdim quant_tp
R 7973 5 65 typh_register_mod stride quant_tp
R 7974 5 66 typh_register_mod quant_address quant_tp
R 7976 5 68 typh_register_mod lowbnd quant_tp
R 7977 5 69 typh_register_mod lowbnd$sd quant_tp
R 7978 5 70 typh_register_mod lowbnd$p quant_tp
R 7979 5 71 typh_register_mod lowbnd$o quant_tp
R 7982 5 74 typh_register_mod uppbnd quant_tp
R 7983 5 75 typh_register_mod uppbnd$sd quant_tp
R 7984 5 76 typh_register_mod uppbnd$p quant_tp
R 7985 5 77 typh_register_mod uppbnd$o quant_tp
R 7989 25 81 typh_register_mod pqll_tp
R 7990 5 82 typh_register_mod phase pqll_tp
R 7991 5 83 typh_register_mod quant pqll_tp
R 7993 5 85 typh_register_mod list pqll_tp
R 7994 5 86 typh_register_mod list$sd pqll_tp
R 7995 5 87 typh_register_mod list$p pqll_tp
R 7996 5 88 typh_register_mod list$o pqll_tp
R 7998 5 90 typh_register_mod nlist pqll_tp
R 7999 5 91 typh_register_mod next pqll_tp
R 8001 5 93 typh_register_mod next$p pqll_tp
R 8281 25 7 typh_key_mod key_set_array_tp
R 8282 5 8 typh_key_mod keyset key_set_array_tp
R 8284 5 10 typh_key_mod keyset$p key_set_array_tp
R 8487 16 1 kinds_mod ink
S 8490 6 4 0 0 6 8491 624 41899 14 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 neltot
S 8491 6 4 0 0 6 8496 624 41906 14 0 A 0 0 0 0 B 0 0 0 0 0 4 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnodtot
S 8492 6 4 0 0 6 8493 624 41914 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 8532 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 viscosity
S 8493 6 4 0 0 6 8494 624 41924 4 0 A 0 0 0 0 B 0 0 0 0 0 4 0 0 0 0 0 0 8532 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 halfstep
S 8494 6 4 0 0 6 8495 624 41933 4 0 A 0 0 0 0 B 0 0 0 0 0 8 0 0 0 0 0 0 8532 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 adv_exch_el
S 8495 6 4 0 0 6 1 624 41945 4 0 A 0 0 0 0 B 0 0 0 0 0 12 0 0 0 0 0 0 8532 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 adv_exch_nd
S 8496 6 4 0 0 6 8497 624 41957 14 0 A 0 0 0 0 B 0 0 0 0 0 8 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 key_comm_cells
S 8497 6 4 0 0 6 8498 624 41972 14 0 A 0 0 0 0 B 0 0 0 0 0 12 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 key_comm_nodes
S 8498 6 4 0 0 6 8499 624 41987 14 0 A 0 0 0 0 B 0 0 0 0 0 16 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 cnmassid
S 8499 6 4 0 0 6 8500 624 41996 14 0 A 0 0 0 0 B 0 0 0 0 0 20 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 cnwtid
S 8500 6 4 0 0 6 8501 624 42003 14 0 A 0 0 0 0 B 0 0 0 0 0 24 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 elfxid
S 8501 6 4 0 0 6 8502 624 42010 14 0 A 0 0 0 0 B 0 0 0 0 0 28 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 elfyid
S 8502 6 4 0 0 6 8503 624 42017 14 0 A 0 0 0 0 B 0 0 0 0 0 32 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rho05id
S 8503 6 4 0 0 6 8504 624 42025 14 0 A 0 0 0 0 B 0 0 0 0 0 36 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 duid
S 8504 6 4 0 0 6 8505 624 42030 14 0 A 0 0 0 0 B 0 0 0 0 0 40 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 dvid
S 8505 6 4 0 0 6 8506 624 42035 14 0 A 0 0 0 0 B 0 0 0 0 0 44 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 dxid
S 8506 6 4 0 0 6 8507 624 42040 14 0 A 0 0 0 0 B 0 0 0 0 0 48 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 dyid
S 8507 6 4 0 0 6 8508 624 42045 14 0 A 0 0 0 0 B 0 0 0 0 0 52 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 dfvid
S 8508 6 4 0 0 6 8509 624 42051 14 0 A 0 0 0 0 B 0 0 0 0 0 56 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 dfmid
S 8509 6 4 0 0 6 8510 624 42057 14 0 A 0 0 0 0 B 0 0 0 0 0 60 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 einid
S 8510 6 4 0 0 6 8511 624 42063 14 0 A 0 0 0 0 B 0 0 0 0 0 64 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rhoid
S 8511 6 4 0 0 6 8512 624 42069 14 0 A 0 0 0 0 B 0 0 0 0 0 68 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 eluvid
S 8512 6 4 0 0 6 8513 624 42076 14 0 A 0 0 0 0 B 0 0 0 0 0 72 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 elvvid
S 8513 6 4 0 0 6 8514 624 42083 14 0 A 0 0 0 0 B 0 0 0 0 0 76 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 elv0id
S 8514 6 4 0 0 6 8515 624 42090 14 0 A 0 0 0 0 B 0 0 0 0 0 80 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 elvolid
S 8515 6 4 0 0 6 8522 624 42098 40800016 0 A 0 0 0 0 B 0 0 0 0 0 84 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_0_4
S 8516 7 6 0 0 4485 1 624 42106 10a00014 51 A 0 0 0 0 B 0 0 0 0 0 0 8518 0 0 0 8520 0 0 0 0 0 0 0 0 8517 0 0 8519 624 0 0 0 0 nodproc
S 8517 8 4 0 0 4488 8524 624 42114 40822014 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nodproc$sd
S 8518 6 4 0 0 7 8519 624 42125 40802011 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nodproc$p
S 8519 6 4 0 0 7 8517 624 42135 40802010 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nodproc$o
S 8520 22 1 0 0 6 1 624 42145 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 8516 0 0 0 0 8517 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nodproc$arrdsc
S 8521 7 6 0 0 4491 1 624 42160 10a00014 51 A 0 0 0 0 B 0 0 0 0 0 0 8524 0 0 0 8526 0 0 0 0 0 0 0 0 8523 0 0 8525 624 0 0 0 0 nnodiel
S 8522 6 4 0 0 6 1 624 42168 40800016 0 A 0 0 0 0 B 0 0 0 0 0 88 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_1_3
S 8523 8 4 0 0 4494 8490 624 42176 40822014 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnodiel$sd
S 8524 6 4 0 0 7 8525 624 42187 40802011 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnodiel$p
S 8525 6 4 0 0 7 8523 624 42197 40802010 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 8531 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnodiel$o
S 8526 22 1 0 0 6 1 624 42207 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 8521 0 0 0 0 8523 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnodiel$arrdsc
S 8527 16 0 0 0 6 1 624 42222 14 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 2 15 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ngstlay
S 8528 27 0 0 0 8 8533 624 42230 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 register
S 8529 27 0 0 0 8 8535 624 42239 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 exchange
S 8530 27 0 0 0 8 8538 624 42248 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 partition_mesh
S 8531 11 0 0 0 8 8416 624 42263 40800010 805000 A 0 0 0 0 B 0 0 0 0 0 268 0 0 8518 8522 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _comms_mod$4
S 8532 11 0 0 0 8 8531 624 42276 40800000 805000 A 0 0 0 0 B 0 0 0 0 0 16 0 0 8492 8495 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _comms_mod$0
S 8533 23 5 0 0 0 8534 624 42230 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 register
S 8534 14 5 0 0 0 1 8533 42230 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5136 0 0 0 0 0 0 0 0 0 0 0 0 0 43 0 624 0 0 0 0 register
F 8534 0
S 8535 23 5 0 0 0 8537 624 42239 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 exchange
S 8536 1 3 1 0 6 1 8535 42289 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm_phase
S 8537 14 5 0 0 0 1 8535 42239 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5137 1 0 0 0 0 0 0 0 0 0 0 0 0 206 0 624 0 0 0 0 exchange
F 8537 1 8536
S 8538 23 5 0 0 0 8542 624 42248 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 partition_mesh
S 8539 6 3 3 0 6 1 8538 42300 800014 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nl
S 8540 6 3 3 0 6 1 8538 42303 800014 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nk
S 8541 1 3 3 0 6 1 8538 42306 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nprocw
S 8542 14 5 0 0 0 1 8538 42248 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5139 3 0 0 0 0 0 0 0 0 0 0 0 0 230 0 624 0 0 0 0 partition_mesh
F 8542 3 8539 8540 8541
S 8543 23 5 0 0 0 8550 624 42313 20010 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rcb
S 8544 6 3 1 0 6 1 8543 42300 800014 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nl
S 8545 6 3 1 0 6 1 8543 42303 800014 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nk
S 8546 1 3 1 0 6 1 8543 42317 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 npartl
S 8547 1 3 1 0 6 1 8543 42324 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nparth
S 8548 1 3 3 0 6 1 8543 42331 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ipart
S 8549 7 3 3 0 4497 1 8543 42337 800214 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 icolour
S 8550 14 5 0 0 0 1 8543 42313 20210 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5143 6 0 0 0 0 0 0 0 0 0 0 0 0 262 0 624 0 0 0 0 rcb
F 8550 6 8544 8545 8546 8547 8548 8549
S 8551 6 1 0 0 6 1 8543 42345 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1727
S 8552 6 1 0 0 6 1 8543 42354 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1728
S 8553 6 1 0 0 6 1 8543 42363 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1731
S 8554 6 1 0 0 6 1 8543 42372 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1733
S 8555 23 5 0 0 0 8560 624 38095 10 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 partition
S 8556 6 3 1 0 6 1 8555 42300 800014 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nl
S 8557 6 3 1 0 6 1 8555 42303 800014 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nk
S 8558 7 3 1 0 4500 1 8555 42337 800214 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 icolour
S 8559 6 3 1 0 6 1 8555 42306 800014 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nprocw
S 8560 14 5 0 0 0 1 8555 38095 210 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5150 4 0 0 0 0 0 0 0 0 0 0 0 0 473 0 624 0 0 0 0 partition
F 8560 4 8556 8557 8558 8559
S 8561 6 1 0 0 6 1 8555 42381 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_2092
S 8562 6 1 0 0 6 1 8555 42390 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_2093
S 8563 6 1 0 0 6 1 8555 42399 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_2096
S 8564 6 1 0 0 6 1 8555 42408 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_2098
S 8565 23 5 0 0 0 8578 624 42417 10 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 get_ghosts
S 8566 1 3 1 0 6 1 8565 42428 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nshape
S 8567 1 3 1 0 6 1 8565 42435 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iproc
S 8568 7 3 3 0 4509 1 8565 42441 10a00014 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8587 0 0 0 0 0 0 0 0 nelghost
S 8569 7 3 3 0 4515 1 8565 42450 10a00014 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8591 0 0 0 0 0 0 0 0 nnodghost
S 8570 7 3 1 0 4503 1 8565 42460 20000014 10003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ielpar
S 8571 7 3 1 0 4506 1 8565 42467 20000014 10003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nnodproc
S 8572 1 3 1 0 6 1 8565 42476 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nel_proc
S 8573 1 3 1 0 6 1 8565 42485 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nnod_proc
S 8574 7 3 3 0 4521 1 8565 42495 10a00014 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8595 0 0 0 0 0 0 0 0 pariel
S 8575 7 3 3 0 4527 1 8565 42502 10a00014 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8599 0 0 0 0 0 0 0 0 parnod
S 8576 7 3 3 0 4533 1 8565 42509 10a00014 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8603 0 0 0 0 0 0 0 0 nodiel
S 8577 7 3 1 0 4539 1 8565 42516 10a00014 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8607 0 0 0 0 0 0 0 0 ielnd
S 8578 14 5 0 0 0 1 8565 42417 20000010 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5155 12 0 0 0 0 0 0 0 0 0 0 0 0 791 0 624 0 0 0 0 get_ghosts
F 8578 12 8566 8567 8568 8569 8570 8571 8572 8573 8574 8575 8576 8577
S 8579 6 1 0 0 6 1 8565 42522 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_0_5
S 8580 6 1 0 0 6 1 8565 42530 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_2_3
S 8581 6 1 0 0 6 1 8565 42538 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_3_3
S 8582 6 1 0 0 6 1 8565 42546 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1755
S 8583 6 1 0 0 6 1 8565 42555 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_4_3
S 8584 6 1 0 0 6 1 8565 42563 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_6_3
S 8585 6 1 0 0 6 1 8565 38401 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_7_2
S 8586 6 1 0 0 6 1 8565 42571 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1762
S 8587 8 1 0 0 4512 1 8565 42580 40822014 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nelghost$sd
S 8591 8 1 0 0 4518 1 8565 42630 40822014 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nnodghost$sd
S 8595 8 1 0 0 4524 1 8565 42684 40822014 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pariel$sd
S 8599 8 1 0 0 4530 1 8565 42726 40822014 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 parnod$sd
S 8603 8 1 0 0 4536 1 8565 42768 40822014 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nodiel$sd
S 8607 8 1 0 0 4542 1 8565 42810 40822014 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ielnd$sd
S 8611 23 5 0 0 0 8629 624 42848 10 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 get_ghost_layer
S 8612 1 3 1 0 6 1 8611 42864 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ilayer
S 8613 1 3 1 0 6 1 8611 42476 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nel_proc
S 8614 1 3 1 0 6 1 8611 42485 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nnod_proc
S 8615 1 3 1 0 6 1 8611 42428 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nshape
S 8616 1 3 3 0 6 1 8611 42871 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nelprev
S 8617 1 3 3 0 6 1 8611 42879 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nnodprev
S 8618 1 3 3 0 6 1 8611 42888 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nelbnd
S 8619 7 3 3 0 4545 1 8611 42895 10a00014 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8630 0 0 0 0 0 0 0 0 iboundlist
S 8620 7 3 3 0 4551 1 8611 42906 20000014 10003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 zelghost
S 8621 7 3 3 0 4554 1 8611 42915 20000014 10003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 znodghost
S 8622 7 3 3 0 4557 1 8611 42502 20000014 10003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 parnod
S 8623 7 3 3 0 4560 1 8611 42495 20000014 10003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pariel
S 8624 7 3 1 0 4563 1 8611 42509 20000014 10003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nodiel
S 8625 7 3 1 0 4569 1 8611 42467 20000014 10003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nnodproc
S 8626 7 3 1 0 4566 1 8611 42516 20000014 10003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ielnd
S 8627 7 3 1 0 4572 1 8611 42460 20000014 10003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ielpar
S 8628 7 3 1 0 4575 1 8611 42925 20000014 10003000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 znodproc
S 8629 14 5 0 0 0 1 8611 42848 20000010 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5168 17 0 0 0 0 0 0 0 0 0 0 0 0 864 0 624 0 0 0 0 get_ghost_layer
F 8629 17 8612 8613 8614 8615 8616 8617 8618 8619 8620 8621 8622 8623 8624 8625 8626 8627 8628
S 8630 8 1 0 0 4548 1 8611 42934 40822014 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iboundlist$sd
S 8634 6 1 0 0 6 1 8611 42555 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_4_3
S 8635 6 1 0 0 6 1 8611 42992 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_5_3
S 8636 6 1 0 0 6 1 8611 42563 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_6_3
S 8637 6 1 0 0 6 1 8611 38401 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_7_2
S 8638 6 1 0 0 6 1 8611 43000 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_9_2
S 8639 6 1 0 0 6 1 8611 38426 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_10_1
S 8640 6 1 0 0 6 1 8611 43008 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1849
S 8641 6 1 0 0 6 1 8611 43017 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1852
S 8642 6 1 0 0 6 1 8611 38435 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_11_1
S 8643 6 1 0 0 6 1 8611 40354 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_12_1
S 8644 6 1 0 0 6 1 8611 11683 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_13
S 8645 6 1 0 0 6 1 8611 11690 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_14
S 8646 6 1 0 0 6 1 8611 11799 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_16
S 8647 6 1 0 0 6 1 8611 11806 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_17
S 8648 6 1 0 0 6 1 8611 43026 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1862
S 8649 6 1 0 0 6 1 8611 43035 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1865
S 8650 6 1 0 0 6 1 8611 11828 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_18
S 8651 6 1 0 0 6 1 8611 11835 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_19
S 8652 6 1 0 0 6 1 8611 11855 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_20
S 8653 6 1 0 0 6 1 8611 11862 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_21
S 8654 6 1 0 0 6 1 8611 11890 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_23
S 8655 6 1 0 0 6 1 8611 11938 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_24
S 8656 6 1 0 0 6 1 8611 43044 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1875
S 8657 6 1 0 0 6 1 8611 43053 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1878
S 8658 6 1 0 0 6 1 8611 11945 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_25
S 8659 6 1 0 0 6 1 8611 11952 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_26
S 8660 6 1 0 0 6 1 8611 11959 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_27
S 8661 6 1 0 0 6 1 8611 11966 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_28
S 8662 6 1 0 0 6 1 8611 11995 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_30
S 8663 6 1 0 0 6 1 8611 12002 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_31
S 8664 6 1 0 0 6 1 8611 43062 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1888
S 8665 6 1 0 0 6 1 8611 43071 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1891
S 8666 6 1 0 0 6 1 8611 12009 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_32
S 8667 6 1 0 0 6 1 8611 12023 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_34
S 8668 6 1 0 0 6 1 8611 12030 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_35
S 8669 6 1 0 0 6 1 8611 12078 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_37
S 8670 6 1 0 0 6 1 8611 12085 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_38
S 8671 6 1 0 0 6 1 8611 43080 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1901
S 8672 6 1 0 0 6 1 8611 43089 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1904
S 8673 6 1 0 0 6 1 8611 12092 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_39
S 8674 6 1 0 0 6 1 8611 12106 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_41
S 8675 6 1 0 0 6 1 8611 12113 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_42
S 8676 6 1 0 0 6 1 8611 12142 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_44
S 8677 6 1 0 0 6 1 8611 12149 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_45
S 8678 6 1 0 0 6 1 8611 43098 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1914
S 8679 6 1 0 0 6 1 8611 43107 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1917
S 8680 6 1 0 0 6 1 8611 12156 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_46
S 8681 6 1 0 0 6 1 8611 12163 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_47
S 8682 6 1 0 0 6 1 8611 12170 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_48
S 8683 6 1 0 0 6 1 8611 12177 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_49
S 8684 6 1 0 0 6 1 8611 43116 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1924
S 8685 6 1 0 0 6 1 8611 12184 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_50
S 8686 6 1 0 0 6 1 8611 12353 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_52
S 8687 6 1 0 0 6 1 8611 12371 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_53
S 8688 6 1 0 0 6 1 8611 43125 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1931
S 8689 6 1 0 0 6 1 8611 12401 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_54
S 8690 6 1 0 0 6 1 8611 12449 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_56
S 8691 6 1 0 0 6 1 8611 12472 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_57
S 8692 6 1 0 0 6 1 8611 43134 40800016 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1938
S 8693 23 5 0 0 0 8699 624 43143 10 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 transfer_partition
S 8694 1 3 1 0 6 1 8693 43162 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nel
S 8695 1 3 1 0 6 1 8693 43166 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nel1
S 8696 1 3 1 0 6 1 8693 43171 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nnod
S 8697 1 3 1 0 6 1 8693 43176 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nnod1
S 8698 1 3 1 0 6 1 8693 43182 14 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nnod2
S 8699 14 5 0 0 0 1 8693 43143 10 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 5186 5 0 0 0 0 0 0 0 0 0 0 0 0 954 0 624 0 0 0 0 transfer_partition
F 8699 5 8694 8695 8696 8697 8698
A 13 2 0 0 0 6 629 0 0 0 13 0 0 0 0 0 0 0 0 0 0
A 15 2 0 0 0 6 630 0 0 0 15 0 0 0 0 0 0 0 0 0 0
A 17 2 0 0 0 6 631 0 0 0 17 0 0 0 0 0 0 0 0 0 0
A 67 1 0 0 0 56 683 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 70 1 0 0 0 65 685 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 77 2 0 0 0 6 717 0 0 0 77 0 0 0 0 0 0 0 0 0 0
A 79 2 0 0 0 6 718 0 0 0 79 0 0 0 0 0 0 0 0 0 0
A 111 2 0 0 0 6 722 0 0 0 111 0 0 0 0 0 0 0 0 0 0
A 117 1 0 1 0 74 744 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 123 1 0 1 0 80 746 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 128 1 0 3 0 86 748 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 152 2 0 0 0 16 762 0 0 0 152 0 0 0 0 0 0 0 0 0 0
A 162 2 0 0 58 6 751 0 0 0 162 0 0 0 0 0 0 0 0 0 0
A 169 2 0 0 0 6 765 0 0 0 169 0 0 0 0 0 0 0 0 0 0
A 172 2 0 0 0 6 766 0 0 0 172 0 0 0 0 0 0 0 0 0 0
A 174 2 0 0 0 6 767 0 0 0 174 0 0 0 0 0 0 0 0 0 0
A 182 2 0 0 0 6 769 0 0 0 182 0 0 0 0 0 0 0 0 0 0
A 198 2 0 0 0 6 772 0 0 0 198 0 0 0 0 0 0 0 0 0 0
A 205 2 0 0 0 6 775 0 0 0 205 0 0 0 0 0 0 0 0 0 0
A 207 2 0 0 0 6 776 0 0 0 207 0 0 0 0 0 0 0 0 0 0
A 254 2 0 0 241 6 793 0 0 0 254 0 0 0 0 0 0 0 0 0 0
A 1217 2 0 0 938 7 7370 0 0 0 1217 0 0 0 0 0 0 0 0 0 0
A 1709 1 0 5 14 4488 8517 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1710 10 0 0 1607 6 1709 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1711 10 0 0 1710 6 1709 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1712 4 0 0 1293 6 1711 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1713 4 0 0 962 6 1710 0 1712 0 0 0 0 1 0 0 0 0 0 0 0
A 1714 10 0 0 1711 6 1709 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1715 10 0 0 1714 6 1709 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1716 10 0 0 1715 6 1709 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 1718 1 0 5 1570 4494 8523 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1719 10 0 0 1259 6 1718 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1720 10 0 0 1719 6 1718 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1721 4 0 0 1173 6 1720 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1722 4 0 0 822 6 1719 0 1721 0 0 0 0 1 0 0 0 0 0 0 0
A 1723 10 0 0 1720 6 1718 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1724 10 0 0 1723 6 1718 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1725 10 0 0 1724 6 1718 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 1727 1 0 0 1122 6 8554 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1728 1 0 0 1414 6 8544 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1729 1 0 0 615 6 8551 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1730 1 0 0 1508 6 8545 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1731 1 0 0 1422 6 8552 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1732 1 0 0 1516 6 8553 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1733 1 0 0 832 6 8564 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1734 1 0 0 376 6 8556 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1735 1 0 0 1634 6 8561 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1736 1 0 0 377 6 8557 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1737 1 0 0 830 6 8562 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1738 1 0 0 1045 6 8563 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1739 1 0 0 1047 6 8581 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1740 1 0 0 1551 6 8579 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1741 1 0 0 838 6 8582 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1742 1 0 0 1546 6 8580 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1743 1 0 0 1548 6 8585 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1744 1 0 0 1000 6 8583 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1745 1 0 0 842 6 8586 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1746 1 0 0 1127 6 8584 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1747 1 0 5 441 4512 8587 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1748 10 0 0 716 6 1747 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 1749 10 0 0 1748 6 1747 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1750 10 0 0 1749 6 1747 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1751 4 0 0 1463 6 1750 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1752 4 0 0 900 6 1749 0 1751 0 0 0 0 1 0 0 0 0 0 0 0
A 1753 10 0 0 1750 6 1747 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1754 10 0 0 1753 6 1747 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1755 1 0 5 1354 4518 8591 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1756 10 0 0 0 6 1755 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 1757 10 0 0 1756 6 1755 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1758 10 0 0 1757 6 1755 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1759 4 0 0 1474 6 1758 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1760 4 0 0 1569 6 1757 0 1759 0 0 0 0 1 0 0 0 0 0 0 0
A 1761 10 0 0 1758 6 1755 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1762 10 0 0 1761 6 1755 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1763 1 0 7 1706 4524 8595 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1764 10 0 0 856 6 1763 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 1765 10 0 0 1764 6 1763 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1766 10 0 0 1765 6 1763 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1767 4 0 0 481 6 1766 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1768 4 0 0 1600 6 1765 0 1767 0 0 0 0 1 0 0 0 0 0 0 0
A 1769 10 0 0 1766 6 1763 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1770 10 0 0 1769 6 1763 16 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 254
A 1771 10 0 0 1770 6 1763 19 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 198
A 1772 4 0 0 0 6 1771 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1773 4 0 0 943 6 1770 0 1772 0 0 0 0 1 0 0 0 0 0 0 0
A 1774 10 0 0 1771 6 1763 22 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 205
A 1775 10 0 0 1774 6 1763 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1776 1 0 7 1473 4530 8599 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1777 10 0 0 1629 6 1776 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 1778 10 0 0 1777 6 1776 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1779 10 0 0 1778 6 1776 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1780 4 0 0 1184 6 1779 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1781 4 0 0 1740 6 1778 0 1780 0 0 0 0 1 0 0 0 0 0 0 0
A 1782 10 0 0 1779 6 1776 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1783 10 0 0 1782 6 1776 16 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 254
A 1784 10 0 0 1783 6 1776 19 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 198
A 1785 4 0 0 1189 6 1784 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1786 4 0 0 1381 6 1783 0 1785 0 0 0 0 1 0 0 0 0 0 0 0
A 1787 10 0 0 1784 6 1776 22 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 205
A 1788 10 0 0 1787 6 1776 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1789 1 0 7 480 4536 8603 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1790 10 0 0 1712 6 1789 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 1791 10 0 0 1790 6 1789 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1792 10 0 0 1791 6 1789 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1793 4 0 0 1188 6 1792 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1794 4 0 0 1389 6 1791 0 1793 0 0 0 0 1 0 0 0 0 0 0 0
A 1795 10 0 0 1792 6 1789 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1796 10 0 0 1795 6 1789 16 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 254
A 1797 10 0 0 1796 6 1789 19 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 198
A 1798 4 0 0 1194 6 1797 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1799 4 0 0 1056 6 1796 0 1798 0 0 0 0 1 0 0 0 0 0 0 0
A 1800 10 0 0 1797 6 1789 22 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 205
A 1801 10 0 0 1800 6 1789 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1802 1 0 7 1092 4542 8607 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1803 10 0 0 1440 6 1802 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 1804 10 0 0 1803 6 1802 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1805 10 0 0 1804 6 1802 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1806 4 0 0 1202 6 1805 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1807 4 0 0 1575 6 1804 0 1806 0 0 0 0 1 0 0 0 0 0 0 0
A 1808 10 0 0 1805 6 1802 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1809 10 0 0 1808 6 1802 16 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 254
A 1810 10 0 0 1809 6 1802 19 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 198
A 1811 4 0 0 1383 6 1810 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1812 4 0 0 1489 6 1809 0 1811 0 0 0 0 1 0 0 0 0 0 0 0
A 1813 10 0 0 1810 6 1802 22 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 205
A 1814 10 0 0 1813 6 1802 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1815 1 0 5 1603 4548 8630 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1816 10 0 0 1455 6 1815 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 1817 10 0 0 1816 6 1815 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 162
A 1818 10 0 0 1817 6 1815 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 169
A 1819 4 0 0 1396 6 1818 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1820 4 0 0 1729 6 1817 0 1819 0 0 0 0 1 0 0 0 0 0 0 0
A 1821 10 0 0 1818 6 1815 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 182
A 1822 10 0 0 1821 6 1815 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 174
A 1823 1 0 0 845 6 8639 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1824 1 0 0 1009 6 8634 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1825 1 0 0 1505 6 8635 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1826 1 0 0 0 6 8640 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1827 1 0 0 896 6 8637 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1828 1 0 0 952 6 8636 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1829 1 0 0 1054 6 8641 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1830 1 0 0 1137 6 8638 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1831 1 0 0 1138 6 8647 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1832 1 0 0 1652 6 8642 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1833 1 0 0 1513 6 8643 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1834 1 0 0 0 6 8648 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1835 1 0 0 953 6 8645 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1836 1 0 0 1010 6 8644 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1837 1 0 0 0 6 8649 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1838 1 0 0 1002 6 8646 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1839 1 0 0 1331 6 8655 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1840 1 0 0 846 6 8650 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1841 1 0 0 1521 6 8651 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1842 1 0 0 1697 6 8656 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1843 1 0 0 1752 6 8653 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1844 1 0 0 1139 6 8652 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1845 1 0 0 1620 6 8657 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1846 1 0 0 1359 6 8654 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1847 1 0 0 1012 6 8663 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1848 1 0 0 1537 6 8658 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1849 1 0 0 1529 6 8659 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1850 1 0 0 0 6 8664 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1851 1 0 0 0 6 8661 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1852 1 0 0 1799 6 8660 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1853 1 0 0 1628 6 8665 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1854 1 0 0 1642 6 8662 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1855 1 0 0 1561 6 8670 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1856 1 0 0 1655 6 8666 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1857 1 0 0 1563 6 8671 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1858 1 0 0 1557 6 8668 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1859 1 0 0 1555 6 8667 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1860 1 0 0 1143 6 8672 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1861 1 0 0 1559 6 8669 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1862 1 0 0 1144 6 8677 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1863 1 0 0 1641 6 8673 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1864 1 0 0 70 6 8678 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1865 1 0 0 1004 6 8675 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1866 1 0 0 850 6 8674 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1867 1 0 0 0 6 8679 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1868 1 0 0 1058 6 8676 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1869 1 0 0 1059 6 8683 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1870 1 0 0 906 6 8680 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1871 1 0 0 1014 6 8681 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1872 1 0 0 1501 6 8684 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1873 1 0 0 1334 6 8682 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1874 1 0 0 1565 6 8687 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1875 1 0 0 1566 6 8685 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1876 1 0 0 1567 6 8688 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1877 1 0 0 1660 6 8686 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1878 1 0 0 1541 6 8691 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1879 1 0 0 1709 6 8689 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1880 1 0 0 1060 6 8692 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1881 1 0 0 1015 6 8690 0 0 0 0 0 0 0 0 0 0 0 0 0 0
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
T 7439 2537 0 3 0 0
A 7445 7 2564 0 1 2 1
A 7446 7 0 0 1 2 1
A 7444 6 0 172 1 2 1
A 7452 7 2566 0 1 2 1
A 7453 7 0 0 1 2 1
A 7451 6 0 172 1 2 1
A 7457 7 2568 0 1 2 1
A 7461 7 2570 0 1 2 1
A 7465 7 2572 0 1 2 0
T 7468 2577 0 3 0 0
A 7479 7 2589 0 1 2 1
A 7483 7 2591 0 1 2 0
T 7486 2596 0 3 0 0
A 7497 7 2614 0 1 2 1
A 7501 7 2616 0 1 2 1
A 7505 7 2618 0 1 2 1
A 7509 7 2620 0 1 2 0
T 7512 2625 0 3 0 0
A 7519 7 2640 0 1 2 1
A 7523 7 2642 0 1 2 1
A 7527 7 2644 0 1 2 0
T 7530 2649 0 3 0 0
A 7547 7 2721 0 1 2 1
A 7548 7 0 0 1 2 1
A 7546 6 0 172 1 2 1
A 7578 7 2723 0 1 2 1
A 7579 7 0 0 1 2 1
A 7577 6 0 172 1 2 1
A 7596 7 2725 0 1 2 1
A 7597 7 0 0 1 2 1
A 7595 6 0 172 1 2 0
T 7607 2824 0 3 0 0
A 7608 6 0 0 1 2 1
A 7609 6 0 0 1 2 1
A 7610 6 0 0 1 2 1
A 7611 6 0 0 1 2 1
A 7612 6 0 0 1 2 1
A 7613 6 0 0 1 2 1
A 7614 6 0 0 1 2 0
T 7617 2833 0 3 0 0
A 7618 6 0 0 1 15 1
A 7619 6 0 0 1 2 1
A 7620 6 0 0 1 2 1
A 7621 6 0 0 1 79 1
A 7622 6 0 0 1 79 1
A 7623 6 0 0 1 79 1
A 7624 6 0 0 1 79 1
A 7625 6 0 0 1 2 1
A 7626 16 0 0 1 152 1
A 7627 16 0 0 1 152 1
A 7628 16 0 0 1 152 0
T 7758 3001 0 3 0 0
A 7765 7 3051 0 1 2 1
A 7766 7 0 0 1 2 1
A 7764 6 0 172 1 2 1
A 7771 7 3053 0 1 2 1
A 7772 7 0 0 1 2 1
A 7770 6 0 172 1 2 1
A 7778 7 3055 0 1 2 1
A 7779 7 0 0 1 2 1
A 7777 6 0 207 1 2 1
A 7785 7 3057 0 1 2 1
A 7786 7 0 0 1 2 1
A 7784 6 0 207 1 2 1
A 7791 7 3059 0 1 2 1
A 7792 7 0 0 1 2 1
A 7790 6 0 172 1 2 1
A 7797 7 3061 0 1 2 1
A 7798 7 0 0 1 2 1
A 7796 6 0 172 1 2 1
A 7804 7 3063 0 1 2 1
A 7805 7 0 0 1 2 1
A 7803 6 0 207 1 2 0
T 7809 3068 0 3 0 0
T 7810 3001 0 3 0 0
A 7765 7 3051 0 1 2 1
A 7766 7 0 0 1 2 1
A 7764 6 0 172 1 2 1
A 7771 7 3053 0 1 2 1
A 7772 7 0 0 1 2 1
A 7770 6 0 172 1 2 1
A 7778 7 3055 0 1 2 1
A 7779 7 0 0 1 2 1
A 7777 6 0 207 1 2 1
A 7785 7 3057 0 1 2 1
A 7786 7 0 0 1 2 1
A 7784 6 0 207 1 2 1
A 7791 7 3059 0 1 2 1
A 7792 7 0 0 1 2 1
A 7790 6 0 172 1 2 1
A 7797 7 3061 0 1 2 1
A 7798 7 0 0 1 2 1
A 7796 6 0 172 1 2 1
A 7804 7 3063 0 1 2 1
A 7805 7 0 0 1 2 1
A 7803 6 0 207 1 2 0
T 7924 3228 0 3 0 0
A 7946 7 3248 0 1 2 1
A 7950 7 3250 0 1 2 0
T 7925 3255 0 3 0 0
A 7969 7 3279 0 1 2 1
A 7970 7 0 0 1 2 1
A 7968 6 0 172 1 2 1
A 7974 7 0 0 1 1217 0
T 7989 3284 0 3 0 0
T 7990 3228 0 3 0 1
A 7946 7 3248 0 1 2 1
A 7950 7 3250 0 1 2 0
T 7991 3255 0 3 0 1
A 7969 7 3279 0 1 2 1
A 7970 7 0 0 1 2 1
A 7968 6 0 172 1 2 1
A 7974 7 0 0 1 1217 0
A 7995 7 3299 0 1 2 1
A 7996 7 0 0 1 2 1
A 7994 6 0 172 1 2 1
A 8001 7 3301 0 1 2 0
T 8281 3659 0 3 0 0
A 8284 7 3668 0 1 2 0
Z

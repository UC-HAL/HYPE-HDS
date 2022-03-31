!> \file hypevar.f90
!> Contains module hypevariables.

!>Variables for the HYPE model (HYdrological Predictions for the Environment)
!!
MODULE HYPEVARIABLES

!Copyright 2011-2020 SMHI
!
!This file is part of HYPE.
!HYPE is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!HYPE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License for more details.
!You should have received a copy of the Lesser GNU General Public License along with HYPE. If not, see <http://www.gnu.org/licenses/>.

!-----------------------------------------------------------------------------------------
!Used by modules
!-----------------------------------------------------------------------------------------
!modelmodule 
!np_processes_module
!oc_processes_module
!irrigation_module
!soil_npc_processes
!soil_processes
!soilmodel_default
!glacier_soilmodel
!-----------------------------------------------------------------------------------------

  SAVE

!General Fortran parameter for HYPE

!> \name Output variable indices
!> \{
  INTEGER,PARAMETER :: o_crun    = 1    !class
  INTEGER,PARAMETER :: o_rrun    = 2    !<Output variable index
  INTEGER,PARAMETER :: o_prec    = 3    !basin
  INTEGER,PARAMETER :: o_tobs    = 4    !basin
  INTEGER,PARAMETER :: o_crunT1  = 5    !class
  INTEGER,PARAMETER :: o_crunT2  = 6    !class
  INTEGER,PARAMETER :: o_roum    = 7    !<Output variable index
  INTEGER,PARAMETER :: o_cprecT1 = 8    !<Output variable index
  INTEGER,PARAMETER :: o_cevapT1 = 9    !class
  INTEGER,PARAMETER :: o_soim    = 10   !class
  INTEGER,PARAMETER :: o_csoilT1 = 11   !class
  INTEGER,PARAMETER :: o_csoilT2 = 12   !class
  INTEGER,PARAMETER :: o_roub    = 13   !<Output variable index
  INTEGER,PARAMETER :: o_snow    = 14   !class
  INTEGER,PARAMETER :: o_evap    = 15   !<Output variable index
  INTEGER,PARAMETER :: o_reT1    = 16
  INTEGER,PARAMETER :: o_reT2    = 17
  INTEGER,PARAMETER :: o_snowdens = 18  !class
  INTEGER,PARAMETER :: o_grwlevel = 19  !class
  INTEGER,PARAMETER :: o_crunIN  = 20   !class
  INTEGER,PARAMETER :: o_crunON  = 21   !class
  INTEGER,PARAMETER :: o_crunSP  = 22   !class
  INTEGER,PARAMETER :: o_crunPP  = 23   !class
  INTEGER,PARAMETER :: o_reIN    = 24   !subbasin
  INTEGER,PARAMETER :: o_reON    = 25   !subbasin
  INTEGER,PARAMETER :: o_reSP    = 26   !subbasin
  INTEGER,PARAMETER :: o_rePP    = 27   !subbasin
  INTEGER,PARAMETER :: o_reTN    = 28   !subbasin
  INTEGER,PARAMETER :: o_reTP    = 29   !subbasin
  INTEGER,PARAMETER :: o_reOC    = 35   !subbasin
  INTEGER,PARAMETER :: o_csoilIN = 36   !class
  INTEGER,PARAMETER :: o_soilfrost = 37   !class
  INTEGER,PARAMETER :: o_soiltmp = 38   !class
  INTEGER,PARAMETER :: o_snowdepth = 39   !class
  INTEGER,PARAMETER :: o_epot    = 40   !class
  INTEGER,PARAMETER :: o_reepot  = 41   !<Output variable index
  INTEGER,PARAMETER :: o_cprc    = 43   !class
  INTEGER,PARAMETER :: o_crunOC  = 44   !class
  INTEGER,PARAMETER :: o_csoilOC = 45   !class
  INTEGER,PARAMETER :: o_wcom    = 51   !outlet lake
  INTEGER,PARAMETER :: o_rewstr  = 52   !<Output variable index
  INTEGER,PARAMETER :: o_cout    = 53   !subbasin
  INTEGER,PARAMETER :: o_rout    = 54   !subbasin
  INTEGER,PARAMETER :: o_crun3   = 65   !class
  INTEGER,PARAMETER :: o_crunTN  = 66   !class
  INTEGER,PARAMETER :: o_crunTP  = 67   !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_pfN    = (/68,69,70/)   !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_phN    = (/71,72,73/)   !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_psoilIN    = (/74,75,76/)   !class
  INTEGER,PARAMETER :: o_ro1     = 79   !class
  INTEGER,PARAMETER :: o_ro2     = 80   !class
  INTEGER,PARAMETER :: o_rod     = 82   !class
  INTEGER,PARAMETER :: o_ros     = 83   !class
  INTEGER,PARAMETER :: o_soildenitr = 84   !class
  INTEGER,PARAMETER :: o_cropNupt = 85   !class
  INTEGER,PARAMETER :: o_degrfN   = 86   !class
  INTEGER,PARAMETER :: o_soilNatm = 87   !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_psoilSP    = (/91,92,93/)   !class
  INTEGER,PARAMETER :: o_soilPatm = 116   !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_pfP = (/94,95,96/)    !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_phP = (/132,133,134/) !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_ppP = (/88,89,90/)    !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_pfC = (/48,50,108/)   !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_phC = (/47,49,109/)   !class
  INTEGER,PARAMETER :: o_cprecIN = 97   !<Output variable index
  INTEGER,PARAMETER :: o_cprecSP = 98   !<Output variable index
  INTEGER,PARAMETER :: o_reswe   = 99   !<Output variable index
  INTEGER,PARAMETER :: o_cloc    = 101  !<Output variable index
  INTEGER,PARAMETER :: o_cTNl    = 110
  INTEGER,PARAMETER :: o_cTPl    = 111
  INTEGER,PARAMETER :: o_ctmp    = 121  !class
  INTEGER,PARAMETER :: o_coum    = 123  !<Output variable index
  INTEGER,PARAMETER :: o_applirr = 126  !class
  INTEGER,PARAMETER :: o_crun2   = 129  !class
  INTEGER,PARAMETER :: o_coub    = 130  !<Output variable index
  INTEGER,PARAMETER :: o_soildef = 131  !class
  INTEGER,PARAMETER :: o_cocl    = 135
  INTEGER,PARAMETER,DIMENSION(3) :: o_psoilON = (/136,137,138/)   !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_sltmp = (/139,140,141/)   !class
  INTEGER,PARAMETER :: o_rainfall= 142  !class
  INTEGER,PARAMETER :: o_snowfall= 143  !class
  INTEGER,PARAMETER :: o_ro3     = 147  !class
  INTEGER,PARAMETER :: o_snowcover = 189   !class
  INTEGER,PARAMETER :: o_snowmax = 191   !class
  INTEGER,PARAMETER :: o_landevap = 195  !class
  INTEGER,PARAMETER :: o_soim12   = 196  !class
  INTEGER,PARAMETER :: o_sml1    = 197  !class
  INTEGER,PARAMETER :: o_sml2    = 198  !class
  INTEGER,PARAMETER :: o_sml3    = 199  !class
  INTEGER,PARAMETER :: o_sml0    = 200  !class
  INTEGER,PARAMETER :: o_smrz    = 201  !class
  INTEGER,PARAMETER :: o_sm13    = 202  !class
  INTEGER,PARAMETER :: o_icloss  = 206  !class och subbasin
  INTEGER,PARAMETER :: o_smffc   = 214  !class
  INTEGER,PARAMETER :: o_smfdep  = 215  !class
  INTEGER,PARAMETER :: o_smrzfdep = 216  !class
  INTEGER,PARAMETER :: o_smfpw   = 217  !class
  INTEGER,PARAMETER :: o_smrzfpw = 218  !class
  INTEGER,PARAMETER :: o_xobsm   = 220  !<Output variable index (220-229)
  INTEGER,PARAMETER :: o_xobss   = 230  !<Output variable index (230-239)
  INTEGER,PARAMETER :: o_specificq    = 247  !<Output variable index
  INTEGER,PARAMETER :: o_ros1 = 275 !class
  INTEGER,PARAMETER :: o_ros2 = 276 !class
  INTEGER,PARAMETER :: o_evapsnow = 278 !class
  INTEGER,PARAMETER :: o_evpt = 279 !class
  INTEGER,PARAMETER :: o_cleanwcom = 280  !outlet lake
  INTEGER,PARAMETER :: o_cleanwstr = 281  !outlet lake
  INTEGER,PARAMETER :: o_wcav = 282
  INTEGER,PARAMETER :: o_psim    = 284  !class
  INTEGER,PARAMETER :: o_soilden3  = 321  !class
  INTEGER,PARAMETER :: o_soildenrz = 322  !class
  INTEGER,PARAMETER :: o_sml9    = 330  !class
  INTEGER,PARAMETER :: o_snowmelt= 331  !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_ppT1    = (/333,334,335/)   !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_psoilT1 = (/336,337,338/)   !class
  INTEGER,PARAMETER,DIMENSION(3) :: o_csoillayerIN = (/327,328,329/)   !class
  INTEGER,PARAMETER :: o_tsmr    = 339  !main river
  INTEGER,PARAMETER :: o_tslr    = 340  !local river
  INTEGER,PARAMETER :: o_T1sf    = 341  !class
  INTEGER,PARAMETER :: o_cro1T1  = 343  !class
  INTEGER,PARAMETER :: o_cro2T1  = 344  !class
  INTEGER,PARAMETER :: o_cro3T1  = 345  !class
  INTEGER,PARAMETER :: o_crodT1  = 346  !class
  INTEGER,PARAMETER :: o_crosT1  = 347  !class
  INTEGER,PARAMETER :: o_crunSS  = 348  !class
  INTEGER,PARAMETER :: o_ccSS    = 349  !subbasin
  INTEGER,PARAMETER :: o_reSS    = 350  !subbasin
  INTEGER,PARAMETER :: o_ccAE    = 351  !subbasin
  INTEGER,PARAMETER :: o_ccTS    = 352  !subbasin
  INTEGER,PARAMETER :: o_dwtr    = 353  !subbasin
  INTEGER,PARAMETER :: o_rpwl    = 354  !subbasin
  INTEGER,PARAMETER :: o_lpwl    = 355  !subbasin
  INTEGER,PARAMETER :: o_gmlt    = 356  !class
  INTEGER,PARAMETER :: o_mrfa    = 357
  INTEGER,PARAMETER :: o_lrfa    = 358
  INTEGER,PARAMETER :: o_mred    = 359
  INTEGER,PARAMETER :: o_lred    = 360
  INTEGER,PARAMETER :: o_cINl    = 361
  INTEGER,PARAMETER :: o_cONl    = 362
  INTEGER,PARAMETER :: o_cSPl    = 363
  INTEGER,PARAMETER :: o_cPPl    = 364
  INTEGER,PARAMETER :: o_cSSl    = 365
  INTEGER,PARAMETER :: o_cTSl    = 366
  INTEGER,PARAMETER :: o_infi    = 367
  INTEGER,PARAMETER :: o_cleanwavg = 368  !outlet lake
  INTEGER,PARAMETER :: o_reTS = 369  !outlet lake
  INTEGER,PARAMETER :: o_clss = 370
  INTEGER,PARAMETER :: o_clts = 371
  INTEGER,PARAMETER :: o_rnlss = 372
  INTEGER,PARAMETER :: o_rnlts = 373
  INTEGER,PARAMETER :: o_wstilake = 374   !internal lake
  INTEGER,PARAMETER :: o_isps = 375   !class
  INTEGER,PARAMETER :: o_ispp = 376   !class
  INTEGER,PARAMETER :: o_psmr = 377
  INTEGER,PARAMETER :: o_pslr = 378
  INTEGER,PARAMETER :: o_ssmr = 379
  INTEGER,PARAMETER :: o_sslr = 380
  INTEGER,PARAMETER :: o_wstiwet  = 381
  INTEGER,PARAMETER :: o_wstowet  = 382
  INTEGER,PARAMETER :: o_iwinfl   = 383
  INTEGER,PARAMETER :: o_iwoutfl  = 384
  INTEGER,PARAMETER :: o_iwutcoin   = 385
  INTEGER,PARAMETER :: o_iwutcoon   = 386
  INTEGER,PARAMETER :: o_iwutcosp   = 387
  INTEGER,PARAMETER :: o_iwutcopp   = 388
  INTEGER,PARAMETER :: o_iwutcoss   = 389
  INTEGER,PARAMETER :: o_iwutcoae   = 390
  INTEGER,PARAMETER :: o_iwutcooc   = 391
  INTEGER,PARAMETER :: o_iwutcot2   = 392
  INTEGER,PARAMETER :: o_iwincoin   = 393
  INTEGER,PARAMETER :: o_iwincoon   = 394
  INTEGER,PARAMETER :: o_iwincosp   = 395
  INTEGER,PARAMETER :: o_iwincopp   = 396
  INTEGER,PARAMETER :: o_iwincoss   = 397
  INTEGER,PARAMETER :: o_iwincoae   = 398
  INTEGER,PARAMETER :: o_iwincooc   = 399
  INTEGER,PARAMETER :: o_iwincot2   = 400
  INTEGER,PARAMETER :: o_iwetvol    = 401
  INTEGER,PARAMETER :: o_owetvol    = 402
  !DG20200128 - for data assimilation "observation function"
  INTEGER,PARAMETER :: o_clrf    = 403  !local runoff in mm/tstep (normalized from cloc (m3/s) by local basin area upstream main river and olake)
  INTEGER,PARAMETER :: o_clrp    = 404  !calculated precipitation on lakes and rivers, mm/tstep
  INTEGER,PARAMETER :: o_clre    = 405  !calculated evaporation from lakes and rivers, mm/tstep
  INTEGER,PARAMETER :: o_qcin    = 406  !calculated local reservoir inflow, m3/s, only calculated as regional outvar, rgqcin = {rgclrf(X)*weights(X) + (rgclpr(Y)+rgclev(Y))*weights(Y)} * (m3/s)/(mm/tstep)
  INTEGER,PARAMETER :: o_qrin    = 407  !recorded   local reservoir inflow, m3/s
  INTEGER,PARAMETER :: o_snht    = 411 !snow heat content
  INTEGER,PARAMETER :: o_snte    = 412 !snow mean temperature
  INTEGER,PARAMETER :: o_snts    = 413 !snow surface temperature
  INTEGER,PARAMETER :: o_dtmp    = 414 !main river ice porosity
  INTEGER,PARAMETER :: o_cmrp    = 415 !main river ice porosity
  INTEGER,PARAMETER :: o_colp    = 416 !olake ice porosity
  INTEGER,PARAMETER :: o_hged    = 417 !river depth
  INTEGER,PARAMETER :: o_snwc    = 418 !liquid water content in snow
  INTEGER,PARAMETER :: o_coul    = 419 !total outflow simple outlet lakes and multi-basin lakes
  !INTEGER,PARAMETER :: o_cout2   = 420 !cout negative
  INTEGER,PARAMETER :: o_c1IN    = 421 !conc main flow
  INTEGER,PARAMETER :: o_c1ON    = 422 !conc main flow
  INTEGER,PARAMETER :: o_c1TN    = 423 !conc main flow
  INTEGER,PARAMETER :: o_c1SP    = 424 !conc main flow
  INTEGER,PARAMETER :: o_c1PP    = 425 !conc main flow
  INTEGER,PARAMETER :: o_c1TP    = 426 !conc main flow
  INTEGER,PARAMETER :: o_c1T1    = 427 !conc main flow
  INTEGER,PARAMETER :: o_c1T2    = 428 !conc main flow
  INTEGER,PARAMETER :: o_c1SS    = 429 !conc main flow
  INTEGER,PARAMETER :: o_c1AE    = 430 !conc main flow
  INTEGER,PARAMETER :: o_c1TS    = 431 !conc main flow
  INTEGER,PARAMETER :: o_c2IN    = 432 !conc branch flow
  INTEGER,PARAMETER :: o_c2ON    = 433 !conc branch flow
  INTEGER,PARAMETER :: o_c2TN    = 434 !conc branch flow
  INTEGER,PARAMETER :: o_c2SP    = 435 !conc branch flow
  INTEGER,PARAMETER :: o_c2PP    = 436 !conc branch flow
  INTEGER,PARAMETER :: o_c2TP    = 437 !conc branch flow
  INTEGER,PARAMETER :: o_c2T1    = 438 !conc branch flow
  INTEGER,PARAMETER :: o_c2T2    = 439 !conc branch flow
  INTEGER,PARAMETER :: o_c2SS    = 440 !conc branch flow
  INTEGER,PARAMETER :: o_c2AE    = 441 !conc branch flow
  INTEGER,PARAMETER :: o_c2TS    = 442 !conc branch flow
  INTEGER,PARAMETER :: o_c3IN    = 443 !conc main flow
  INTEGER,PARAMETER :: o_c3ON    = 444 !conc main flow
  INTEGER,PARAMETER :: o_c3TN    = 445 !conc main flow
  INTEGER,PARAMETER :: o_c3SP    = 446 !conc main flow
  INTEGER,PARAMETER :: o_c3PP    = 447 !conc main flow
  INTEGER,PARAMETER :: o_c3TP    = 448 !conc main flow
  INTEGER,PARAMETER :: o_c3T1    = 449 !conc main flow
  INTEGER,PARAMETER :: o_c3T2    = 450 !conc main flow
  INTEGER,PARAMETER :: o_c3SS    = 451 !conc main flow
  INTEGER,PARAMETER :: o_c3AE    = 452 !conc main flow
  INTEGER,PARAMETER :: o_c3TS    = 453 !conc main flow
  INTEGER,PARAMETER :: o_c4IN    = 454 !conc branch flow
  INTEGER,PARAMETER :: o_c4ON    = 455 !conc branch flow
  INTEGER,PARAMETER :: o_c4TN    = 456 !conc branch flow
  INTEGER,PARAMETER :: o_c4SP    = 457 !conc branch flow
  INTEGER,PARAMETER :: o_c4PP    = 458 !conc branch flow
  INTEGER,PARAMETER :: o_c4TP    = 459 !conc branch flow
  INTEGER,PARAMETER :: o_c4T1    = 460 !conc branch flow
  INTEGER,PARAMETER :: o_c4T2    = 461 !conc branch flow
  INTEGER,PARAMETER :: o_c4SS    = 462 !conc branch flow
  INTEGER,PARAMETER :: o_c4AE    = 463 !conc branch flow
  INTEGER,PARAMETER :: o_c4TS    = 464 !conc branch flow
  INTEGER,PARAMETER :: o_c5IN    = 465 !conc in ilake
  INTEGER,PARAMETER :: o_c5ON    = 466 !conc in ilake
  INTEGER,PARAMETER :: o_c5TN    = 467 !conc in ilake
  INTEGER,PARAMETER :: o_c5SP    = 468 !conc in ilake
  INTEGER,PARAMETER :: o_c5PP    = 469 !conc in ilake
  INTEGER,PARAMETER :: o_c5TP    = 470 !conc in ilake
  INTEGER,PARAMETER :: o_c5T1    = 471 !conc in ilake
  INTEGER,PARAMETER :: o_c5T2    = 472 !conc in ilake
  INTEGER,PARAMETER :: o_c5SS    = 473 !conc in ilake
  INTEGER,PARAMETER :: o_c5AE    = 474 !conc in ilake
  INTEGER,PARAMETER :: o_c5TS    = 475 !conc in ilake
  INTEGER,PARAMETER :: o_c6IN    = 476 !conc in olake
  INTEGER,PARAMETER :: o_c6ON    = 477 !conc in olake
  INTEGER,PARAMETER :: o_c6TN    = 478 !conc in olake
  INTEGER,PARAMETER :: o_c6SP    = 479 !conc in olake
  INTEGER,PARAMETER :: o_c6PP    = 480 !conc in olake
  INTEGER,PARAMETER :: o_c6TP    = 481 !conc in olake
  INTEGER,PARAMETER :: o_c6T1    = 482 !conc in olake
  INTEGER,PARAMETER :: o_c6T2    = 483 !conc in olake
  INTEGER,PARAMETER :: o_c6SS    = 484 !conc in olake
  INTEGER,PARAMETER :: o_c6AE    = 485 !conc in olake
  INTEGER,PARAMETER :: o_c6TS    = 486 !conc in olake
  INTEGER,PARAMETER :: o_c1OC    = 487 !conc main flow
  INTEGER,PARAMETER :: o_c2OC    = 488 !conc branch flow
  INTEGER,PARAMETER :: o_c3OC    = 489 !conc main flow
  INTEGER,PARAMETER :: o_c4OC    = 490 !conc branch flow
  INTEGER,PARAMETER :: o_c5OC    = 491 !conc in ilake
  INTEGER,PARAMETER :: o_c6OC    = 492 !conc in olake
  INTEGER,PARAMETER :: o_crgl    = 493 !global radiation
  INTEGER,PARAMETER :: o_crnt    = 494 !net radiation
  INTEGER,PARAMETER :: o_cmrr    = 495 !main river global radiation
  INTEGER,PARAMETER :: o_crpt    = 496 !potential global radiation (clearsky)
  INTEGER,PARAMETER :: o_crex    = 497 !extraterrestrial radiation
  INTEGER,PARAMETER :: o_reAE    = 498
  INTEGER,PARAMETER :: o_fnca    = 499  !fraction of non-contributing sub-basin area
  INTEGER,PARAMETER :: o_fcon    = 500  !fraction of ilake connectivity
  INTEGER,PARAMETER :: o_aqinconc    = 501  !aquifer IN conc
  INTEGER,PARAMETER :: o_aqonconc    = 502  !aquifer ON conc
  INTEGER,PARAMETER :: o_aqspconc    = 503  !aquifer SP conc
  INTEGER,PARAMETER :: o_aqppconc    = 504  !aquifer PP conc
  INTEGER,PARAMETER :: o_aqssconc    = 505  !aquifer SS conc
  INTEGER,PARAMETER :: o_aqt1conc    = 506  !aquifer T1 conc
  INTEGER,PARAMETER :: o_aqocconc    = 507  !aquifer OC conc
  INTEGER,PARAMETER :: o_c7IN    = 508 !conc lake outflow
  INTEGER,PARAMETER :: o_c7ON    = 509 !conc lake outflow
  INTEGER,PARAMETER :: o_c7TN    = 510 !conc lake outflow
  INTEGER,PARAMETER :: o_c7SP    = 511 !conc lake outflow
  INTEGER,PARAMETER :: o_c7PP    = 512 !conc lake outflow
  INTEGER,PARAMETER :: o_c7TP    = 513 !conc lake outflow
  INTEGER,PARAMETER :: o_c7OC    = 514 !conc lake outflow
  INTEGER,PARAMETER :: o_c7T1    = 515 !conc lake outflow
  INTEGER,PARAMETER :: o_c7T2    = 516 !conc lake outflow
  INTEGER,PARAMETER :: o_c7SS    = 517 !conc lake outflow
  INTEGER,PARAMETER :: o_c7AE    = 518 !conc lake outflow
  INTEGER,PARAMETER :: o_c7TS    = 519 !conc lake outflow
  INTEGER,PARAMETER :: o_c8IN    = 520 !conc lake outflow
  INTEGER,PARAMETER :: o_c8ON    = 521 !conc lake outflow
  INTEGER,PARAMETER :: o_c8TN    = 522 !conc lake outflow
  INTEGER,PARAMETER :: o_c8SP    = 523 !conc lake outflow
  INTEGER,PARAMETER :: o_c8PP    = 524 !conc lake outflow
  INTEGER,PARAMETER :: o_c8TP    = 525 !conc lake outflow
  INTEGER,PARAMETER :: o_c8OC    = 526 !conc lake outflow
  INTEGER,PARAMETER :: o_c8T1    = 527 !conc lake outflow
  INTEGER,PARAMETER :: o_c8T2    = 528 !conc lake outflow
  INTEGER,PARAMETER :: o_c8SS    = 529 !conc lake outflow
  INTEGER,PARAMETER :: o_c8AE    = 530 !conc lake outflow
  INTEGER,PARAMETER :: o_c8TS    = 531 !conc lake outflow
  !Maximum number of outvar
  INTEGER,PARAMETER :: o_max_outvar = 531     !number of output variables

!>\}

!> \name Model parameter identification variable indices
!> \{
  INTEGER,PARAMETER :: n_lp         = 1    !<Parameter variable index
  INTEGER,PARAMETER :: n_cevpa      = 2    !<Parameter variable index
  INTEGER,PARAMETER :: n_cevpp      = 3    !<Parameter variable index
  INTEGER,PARAMETER :: n_fastn0     = 5
  INTEGER,PARAMETER :: n_fastp0     = 6
  INTEGER,PARAMETER :: n_iniT2      = 8
  INTEGER,PARAMETER :: n_T1evap     = 9    !<Parameter variable index
  INTEGER,PARAMETER :: n_rivv       = 10   !<Parameter variable index
  INTEGER,PARAMETER :: n_damp       = 11   !<Parameter variable index
  INTEGER,PARAMETER :: n_tcalt      = 12   !<Parameter variable index
  INTEGER,PARAMETER :: n_denitrlu   = 15   !<Parameter variable index
  INTEGER,PARAMETER :: n_dissolfn   = 18
  INTEGER,PARAMETER :: n_dissolfp   = 19
  INTEGER,PARAMETER :: n_tcea       = 21   !<Parameter variable index
  INTEGER,PARAMETER :: n_pcem       = 22   !<Parameter variable index
  INTEGER,PARAMETER :: n_tcorr      = 23   !<Parameter variable index
  INTEGER,PARAMETER :: n_pcea       = 24   !<Parameter variable index
  INTEGER,PARAMETER :: n_epotdist   = 26   !<Parameter variable index
  INTEGER,PARAMETER :: n_pcet       = 29   !<Parameter variable index
  INTEGER,PARAMETER :: n_ttmp       = 31   !<Parameter variable index
  INTEGER,PARAMETER :: n_cmlt       = 32   !<Parameter variable index
  INTEGER,PARAMETER :: n_cevp       = 33   !<Parameter variable index
  INTEGER,PARAMETER :: n_srrcs      = 35
  INTEGER,PARAMETER :: n_ttpd       = 36   !<Parameter variable index
  INTEGER,PARAMETER :: n_ttpi       = 37   !<Parameter variable index
  INTEGER,PARAMETER :: n_dissolhn   = 38
  INTEGER,PARAMETER :: n_dissolhp   = 39
  INTEGER,PARAMETER :: n_humusn0    = 40
  INTEGER,PARAMETER :: n_partp0     = 41
  INTEGER,PARAMETER :: n_wsfluse    = 42
  INTEGER,PARAMETER :: n_wsfscale   = 43
  INTEGER,PARAMETER :: n_wsfbias    = 44
  INTEGER,PARAMETER :: n_sfdmax     = 45
  INTEGER,PARAMETER :: n_numdir     = 46
  INTEGER,PARAMETER :: n_wcfc       = 47   !<Parameter variable index
  INTEGER,PARAMETER :: n_wcwp       = 48   !<Parameter variable index
  INTEGER,PARAMETER :: n_wcep       = 49   !<Parameter variable index
  INTEGER,PARAMETER :: n_wcfc1      = 50   !<Parameter variable index
  INTEGER,PARAMETER :: n_wcwp1      = 51   !<Parameter variable index
  INTEGER,PARAMETER :: n_wcep1      = 52   !<Parameter variable index
  INTEGER,PARAMETER :: n_wcfc2      = 53   !<Parameter variable index
  INTEGER,PARAMETER :: n_wcwp2      = 54   !<Parameter variable index
  INTEGER,PARAMETER :: n_wcep2      = 55   !<Parameter variable index
  INTEGER,PARAMETER :: n_wcfc3      = 56   !<Parameter variable index
  INTEGER,PARAMETER :: n_wcwp3      = 57   !<Parameter variable index
  INTEGER,PARAMETER :: n_wcep3      = 58   !<Parameter variable index
  INTEGER,PARAMETER :: n_depthrel   = 66
  INTEGER,PARAMETER :: n_minerfn    = 71
  INTEGER,PARAMETER :: n_minerfp    = 72
  INTEGER,PARAMETER :: n_degradhn   = 73
  INTEGER,PARAMETER :: n_deepmem    = 74
  INTEGER,PARAMETER :: n_surfmem    = 75
  INTEGER,PARAMETER :: n_freuc      = 76
  INTEGER,PARAMETER :: n_freuexp    = 77
  INTEGER,PARAMETER :: n_freurate   = 78
  INTEGER,PARAMETER :: n_rcgrw      = 84
  INTEGER,PARAMETER :: n_rrcs3      = 87   !<Parameter variable index
  INTEGER,PARAMETER :: n_hnhalf     = 88
  INTEGER,PARAMETER :: n_pphalf     = 89
  INTEGER,PARAMETER :: n_filtPbuf   = 91
  INTEGER,PARAMETER :: n_filtPinner = 92
  INTEGER,PARAMETER :: n_filtPother = 93
  INTEGER,PARAMETER :: n_drypp      = 94
  INTEGER,PARAMETER :: n_wetsp      = 95
  INTEGER,PARAMETER :: n_srrate     = 102
  INTEGER,PARAMETER :: n_macrate    = 103
  INTEGER,PARAMETER :: n_mactrinf   = 104
  INTEGER,PARAMETER :: n_mactrsm    = 105
  INTEGER,PARAMETER :: n_soilcoh    = 106
  INTEGER,PARAMETER :: n_soilerod   = 107
  INTEGER,PARAMETER :: n_sreroexp   = 109
  INTEGER,PARAMETER :: n_dsndens    = 110
  INTEGER,PARAMETER :: n_macfilt    = 112
  INTEGER,PARAMETER :: n_fertdays   = 113
  INTEGER,PARAMETER :: n_humusp0    = 114
  INTEGER,PARAMETER :: n_hphalf     = 115
  INTEGER,PARAMETER :: n_degradhp   = 116
  INTEGER,PARAMETER :: n_cevpc      = 117  !<Parameter variable index
  INTEGER,PARAMETER :: n_sndens0    = 131
  INTEGER,PARAMETER :: n_pprelmax   = 132
  INTEGER,PARAMETER :: n_pprelexp   = 133
  INTEGER,PARAMETER :: n_rrcsc      = 140  !<Parameter variable index
  INTEGER,PARAMETER :: n_limprod    = 144
  INTEGER,PARAMETER :: n_partp1     = 145
  INTEGER,PARAMETER :: n_partp2     = 146
  INTEGER,PARAMETER :: n_partp3     = 147
  INTEGER,PARAMETER :: n_limt2exch  = 159
  INTEGER,PARAMETER :: n_sfdlim     = 160
  INTEGER,PARAMETER :: n_laketemp   = 168
  INTEGER,PARAMETER :: n_incorr     = 170
  INTEGER,PARAMETER :: n_oncorr     = 171
  INTEGER,PARAMETER :: n_phoscorr   = 172
  INTEGER,PARAMETER :: n_ponatm     = 174
  INTEGER,PARAMETER :: n_onpercred  = 182
  INTEGER,PARAMETER :: n_pppercred  = 183
  INTEGER,PARAMETER :: n_onconc0    = 184
  INTEGER,PARAMETER :: n_ppconc0    = 185
  INTEGER,PARAMETER :: n_occonc0    = 186
  INTEGER,PARAMETER :: n_snalbmin   = 187  !<Parameter variable index
  INTEGER,PARAMETER :: n_snalbmax   = 188  !<Parameter variable index
  INTEGER,PARAMETER :: n_snalbkexp  = 189  !<Parameter variable index
  INTEGER,PARAMETER :: n_cmrad      = 190  !<Parameter variable index
  INTEGER,PARAMETER :: n_t2trriver  = 193
  INTEGER,PARAMETER :: n_t2trlake   = 194
  INTEGER,PARAMETER :: n_licetf     = 198
  INTEGER,PARAMETER :: n_licesndens = 199
  INTEGER,PARAMETER :: n_licekika   = 200
  INTEGER,PARAMETER :: n_licekexp   = 201
  INTEGER,PARAMETER :: n_licetmelt  = 202
  INTEGER,PARAMETER :: n_ricetf     = 205
  INTEGER,PARAMETER :: n_ricesndens = 206
  INTEGER,PARAMETER :: n_ricekika   = 207
  INTEGER,PARAMETER :: n_ricekexp   = 208
  INTEGER,PARAMETER :: n_ricetmelt  = 209
  INTEGER,PARAMETER :: n_krs        = 218  !<Parameter variable index
  INTEGER,PARAMETER :: n_jhtadd     = 219  !<Parameter variable index
  INTEGER,PARAMETER :: n_jhtscale   = 220  !<Parameter variable index
  INTEGER,PARAMETER :: n_alfapt     = 221  !<Parameter variable index
  INTEGER,PARAMETER :: n_mwind      = 222  !<Parameter variable index
  INTEGER,PARAMETER,DIMENSION(5) :: n_kc = (/223,310,311,312,313/)
  INTEGER,PARAMETER :: n_alb        = 224  !<Parameter variable index
  INTEGER,PARAMETER :: n_glacvcoef  = 225
  INTEGER,PARAMETER :: n_glacvexp   = 226
  INTEGER,PARAMETER :: n_glacdens   = 227
  INTEGER,PARAMETER :: n_glacvcoef1 = 228
  INTEGER,PARAMETER :: n_glacvexp1  = 229
  INTEGER,PARAMETER :: n_rcgrwst    = 231
  INTEGER,PARAMETER :: n_aqretcorr  = 232
  INTEGER,PARAMETER :: n_aqdelcorr  = 233
  INTEGER,PARAMETER :: n_aqpercorr  = 234
  INTEGER,PARAMETER :: n_denitaq    = 235
  INTEGER,PARAMETER :: n_tcfriver   = 236
  INTEGER,PARAMETER :: n_scfriver   = 237
  INTEGER,PARAMETER :: n_ccfriver   = 238
  INTEGER,PARAMETER :: n_lcfriver   = 239
  INTEGER,PARAMETER :: n_tcflake    = 240
  INTEGER,PARAMETER :: n_scflake    = 241
  INTEGER,PARAMETER :: n_ccflake    = 242
  INTEGER,PARAMETER :: n_lcflake    = 243
  INTEGER,PARAMETER :: n_stbcorr1   = 244
  INTEGER,PARAMETER :: n_stbcorr2   = 245
  INTEGER,PARAMETER :: n_stbcorr3   = 246
  INTEGER,PARAMETER :: n_zwind      = 247  !<Parameter variable index
  INTEGER,PARAMETER :: n_zwish      = 248  !<Parameter variable index
  INTEGER,PARAMETER :: n_zpdh       = 249  !<Parameter variable index
  INTEGER,PARAMETER :: n_roughness  = 250  !<Parameter variable index
  INTEGER,PARAMETER :: n_pcur       = 252  !<Parameter variable index
  INTEGER,PARAMETER :: n_pcus       = 253  !<Parameter variable index
  INTEGER,PARAMETER :: n_fepotsnow  = 254
  INTEGER,PARAMETER :: n_cmrefr     = 255  !<Parameter variable index
  INTEGER,PARAMETER :: n_fsceff     = 256  !<Parameter variable index
  INTEGER,PARAMETER :: n_sdnsmax    = 278
  INTEGER,PARAMETER :: n_sdnsrate   = 279
  INTEGER,PARAMETER :: n_sdnsradd   = 280
  INTEGER,PARAMETER :: n_hsatINsoil = 284
  INTEGER,PARAMETER :: n_denit3reg  = 294
  INTEGER,PARAMETER :: n_ttrig      = 296  !<Parameter variable index
  INTEGER,PARAMETER :: n_treda      = 297  !<Parameter variable index
  INTEGER,PARAMETER :: n_tredb      = 298  !<Parameter variable index
  INTEGER,PARAMETER :: n_gldepo     = 299
  INTEGER,PARAMETER :: n_bfroznsoil = 309
  INTEGER,PARAMETER :: n_erodluse   = 314
  INTEGER,PARAMETER :: n_erodsoil   = 315
  INTEGER,PARAMETER :: n_erodslope  = 316
  INTEGER,PARAMETER :: n_erodexp    = 317
  INTEGER,PARAMETER :: n_erodindex  = 318
  INTEGER,PARAMETER :: n_sedae      = 321  !<Parameter variable index
  INTEGER,PARAMETER :: n_wetspl     = 322
  INTEGER,PARAMETER :: n_fraxe      = 323
  INTEGER,PARAMETER :: n_fraxm      = 324
  INTEGER,PARAMETER :: n_ppenrmax   = 325
  INTEGER,PARAMETER :: n_ppenrstab  = 326
  INTEGER,PARAMETER :: n_ppenrflow  = 327
  INTEGER,PARAMETER :: n_hygeomf    = 339
  INTEGER,PARAMETER :: n_hygeomm    = 340
  INTEGER,PARAMETER :: n_hygeomc    = 341
  INTEGER,PARAMETER :: n_hygeomg    = 342
  INTEGER,PARAMETER :: n_licessmft  = 348 !<Parameter variable index 
  INTEGER,PARAMETER :: n_licessmfr  = 349 !<Parameter variable index 
  INTEGER,PARAMETER :: n_licermelt  = 350 !<Parameter variable index 
  INTEGER,PARAMETER :: n_licebupo   = 351 !<Parameter variable index
  INTEGER,PARAMETER :: n_liceqhw    = 352 !<Parameter variable index
  INTEGER,PARAMETER :: n_ricessmft  = 353 !<Parameter variable index 
  INTEGER,PARAMETER :: n_ricessmfr  = 354 !<Parameter variable index 
  INTEGER,PARAMETER :: n_ricermelt  = 355 !<Parameter variable index 
  INTEGER,PARAMETER :: n_ricebupo   = 356 !<Parameter variable index
  INTEGER,PARAMETER :: n_riceqhmn   = 357 !<Parameter variable index
  INTEGER,PARAMETER :: n_riceqhmx   = 358 !<Parameter variable index
  INTEGER,PARAMETER :: n_ricecwi    = 359 !<Parameter variable index
  INTEGER,PARAMETER :: n_snkika     = 360 !<Parameter variable index
  INTEGER,PARAMETER :: n_logsatmp   = 361 !<Parameter variable index 
  INTEGER,PARAMETER :: n_bcosby     = 362 !<Parameter variable index
  INTEGER,PARAMETER :: n_fzsexpand  = 363 !<Parameter variable index
  INTEGER,PARAMETER :: n_whcsnow    = 364
  INTEGER,PARAMETER :: n_ricethpo   = 365 !<Parameter variable index
  INTEGER,PARAMETER :: n_muptnriv   = 418
  INTEGER,PARAMETER :: n_muptpriv   = 419
  INTEGER,PARAMETER :: n_muptrivdep = 420
  INTEGER,PARAMETER :: n_macfrac    = 421
  INTEGER,PARAMETER :: n_srbeta     = 422
  INTEGER,PARAMETER :: n_sralpha    = 423
  INTEGER,PARAMETER :: n_srgamma    = 424
  INTEGER,PARAMETER :: n_srnlayer   = 425
  INTEGER,PARAMETER :: n_spdecaq    = 426
  INTEGER,PARAMETER :: n_ppdecaq    = 427  
  INTEGER,PARAMETER :: n_ssdecaq    = 428
  INTEGER,PARAMETER :: n_aedecaq    = 429
  INTEGER,PARAMETER :: n_t1decaq    = 430 
  INTEGER,PARAMETER :: n_ondecaq    = 431 
  INTEGER,PARAMETER :: n_ocdecaq    = 432 
  INTEGER,PARAMETER :: n_ricew0por  = 433 !<Parameter variable index
  INTEGER,PARAMETER :: n_ricew1por  = 434 !<Parameter variable index
  INTEGER,PARAMETER :: n_ricew0ice  = 435 !<Parameter variable index
  INTEGER,PARAMETER :: n_ricew1ice  = 436 !<Parameter variable index
  INTEGER,PARAMETER :: n_max_par    = 437    !<Maximum of Parameters
!>\}

!Indices for model parameters
!> \name General model parameter indices
!> \{
  INTEGER,PARAMETER :: m_lp        = 1  !<General model parameter index
  INTEGER,PARAMETER :: m_cevpam    = 2  !<General model parameter index 
  INTEGER,PARAMETER :: m_cevpph    = 3  !<General model parameter index 
  INTEGER,PARAMETER :: m_deadl     = 4  !<General model parameter index 
  INTEGER,PARAMETER :: m_fastN0    = 5  !<General model parameter index  
  INTEGER,PARAMETER :: m_fastP0    = 6  !<General model parameter index  
  INTEGER,PARAMETER :: m_iniT1     = 7  !<General model parameter index
  INTEGER,PARAMETER :: m_iniT2     = 8  !<General model parameter index
  INTEGER,PARAMETER :: m_T1evap    = 9  !<General model parameter index 
  INTEGER,PARAMETER :: m_rivvel    = 10 !<General model parameter index 
  INTEGER,PARAMETER :: m_damp      = 11 !<General model parameter index 
  INTEGER,PARAMETER :: m_tcalt     = 12 !<General model parameter index 
  INTEGER,PARAMETER :: m_grat1     = 13 !<General model parameter index 
  INTEGER,PARAMETER :: m_grat2     = 14 !<General model parameter index 
  INTEGER,PARAMETER :: m_deadm     = 15 !<General model parameter index 
  INTEGER,PARAMETER :: m_denitwr   = 16 !<General model parameter index 
  INTEGER,PARAMETER :: m_limprod   = 17 !<General model parameter index 
  INTEGER,PARAMETER :: m_denitwl   = 18 !<General model parameter index 
  INTEGER,PARAMETER :: m_laketemp  = 19 !<General model parameter index 
  INTEGER,PARAMETER :: m_littdays  = 20 !<General model parameter index 
  INTEGER,PARAMETER :: m_denitwrl  = 21 !<General model parameter index 
  INTEGER,PARAMETER :: m_sndens0   = 22 !<General model parameter index 
  INTEGER,PARAMETER :: m_dsndens   = 23 !<General model parameter index 
  INTEGER,PARAMETER :: m_pprelmax  = 24 !<General model parameter index 
  INTEGER,PARAMETER :: m_pprelexp  = 25 !<General model parameter index 
  INTEGER,PARAMETER :: m_epotdist  = 26 !<General model parameter index 
  !INTEGER,PARAMETER :: m_fastlake  = 27 !<General model parameter index 
  INTEGER,PARAMETER :: m_wprodn    = 28 !<General model parameter index 
  INTEGER,PARAMETER :: m_wprodc    = 29 !<General model parameter index 
  INTEGER,PARAMETER :: m_wprodp    = 30 !<General model parameter index 
  INTEGER,PARAMETER :: m_sedon     = 31 !<General model parameter index 
  INTEGER,PARAMETER :: m_sedoc     = 32 !<General model parameter index 
  INTEGER,PARAMETER :: m_sedpp     = 33 !<General model parameter index 
  INTEGER,PARAMETER :: m_sedexp    = 34 !<General model parameter index 
  INTEGER,PARAMETER :: m_rcgrw     = 35 !<General model parameter index 
  INTEGER,PARAMETER :: m_gldepi    = 36 !<General model parameter index 
  INTEGER,PARAMETER :: m_locsoil   = 37 !<General model parameter index 
  INTEGER,PARAMETER :: m_qmean     = 38 !<General model parameter index 
  INTEGER,PARAMETER :: m_deepmem   = 39 !<General model parameter index 
  INTEGER,PARAMETER :: m_wetsp     = 40 !<General model parameter index 
  INTEGER,PARAMETER :: m_sreroexp  = 41 !<General model parameter index 
  INTEGER,PARAMETER :: m_fertdays  = 42 !<General model parameter index 
  INTEGER,PARAMETER :: m_sfdlim    = 43 !<General model parameter index 
  INTEGER,PARAMETER :: m_rrcs3     = 44 !<General model parameter index 
  INTEGER,PARAMETER :: m_pcaddg    = 45 !<General model parameter index 
  INTEGER,PARAMETER :: m_pcelevth  = 46 !<General model parameter index 
  INTEGER,PARAMETER :: m_minc      = 47 !<General model parameter index 
  INTEGER,PARAMETER :: m_crate1    = 48 !<General model parameter index 
  INTEGER,PARAMETER :: m_crate2    = 49 !<General model parameter index  
  INTEGER,PARAMETER :: m_crate3    = 50 !<General model parameter index 
  INTEGER,PARAMETER :: m_ripe      = 52 !<General model parameter index 
  INTEGER,PARAMETER :: m_rips      = 53 !<General model parameter index 
  INTEGER,PARAMETER :: m_crate5    = 54 !<General model parameter index   
  INTEGER,PARAMETER :: m_crate6    = 55 !<General model parameter index  
  INTEGER,PARAMETER :: m_crate9    = 56 !<General model parameter index 
  INTEGER,PARAMETER :: m_crate10   = 57 !<General model parameter index 
  INTEGER,PARAMETER :: m_maxwidth  = 58 !<General model parameter index 
  INTEGER,PARAMETER :: m_pcelevadd = 60 !<General model parameter index 
  INTEGER,PARAMETER :: m_pcelevmax = 61 !<General model parameter index 
  INTEGER,PARAMETER :: m_tcelevadd = 62 !<General model parameter index 
  INTEGER,PARAMETER :: m_regirr    = 63 !<General model parameter index 
  INTEGER,PARAMETER :: m_sswcorr   = 64 !<General model parameter index 
  INTEGER,PARAMETER :: m_immdep    = 65 !<General model parameter index 
  INTEGER,PARAMETER :: m_iwdfrac   = 66 !<General model parameter index 
  INTEGER,PARAMETER :: m_wdpar     = 67 !<General model parameter index 
  INTEGER,PARAMETER :: m_ttpd      = 68 !<General model parameter index 
  INTEGER,PARAMETER :: m_ttpi      = 69 !<General model parameter index 
  INTEGER,PARAMETER :: m_irrcomp   = 70 !<General model parameter index 
  INTEGER,PARAMETER :: m_tcobselev = 71 !<General model parameter index 
  INTEGER,PARAMETER :: m_atmload   = 72 !<General model parameter index
  INTEGER,PARAMETER :: m_denitaq   = 73 !<General model parameter index
  INTEGER,PARAMETER :: m_krelflood = 74 !<General model parameter index
!--Lake and river water parameters  
  INTEGER,PARAMETER :: m_t2trriver = 75 !<General model parameter index
  INTEGER,PARAMETER :: m_upper2deep= 76 !<General model parameter index
  INTEGER,PARAMETER :: m_t2trlake  = 77 !<General model parameter index
!---Lake ice parameters
  INTEGER,PARAMETER :: m_licewme   = 78 !<General model parameter index
  INTEGER,PARAMETER :: m_liceTF    = 79 !<General model parameter index
  INTEGER,PARAMETER :: m_licesndens= 80 !<General model parameter index
  INTEGER,PARAMETER :: m_licekika  = 81 !<General model parameter index
  INTEGER,PARAMETER :: m_licekexp  = 82 !<General model parameter index
  INTEGER,PARAMETER :: m_licetmelt = 83 !<General model parameter index
!---River ice parameter
  INTEGER,PARAMETER :: m_ricewme   = 84 !<General model parameter index
  INTEGER,PARAMETER :: m_riceTF    = 85 !<General model parameter index
  INTEGER,PARAMETER :: m_ricesndens= 86 !<General model parameter index
  INTEGER,PARAMETER :: m_ricekika  = 87 !<General model parameter index
  INTEGER,PARAMETER :: m_ricekexp  = 88 !<General model parameter index
  INTEGER,PARAMETER :: m_ricetmelt = 89 !<General model parameter index
  INTEGER,PARAMETER :: m_licewcorr = 90 !<General model parameter index
!--Snow cover fraction, general
  INTEGER,PARAMETER :: m_fscmax    = 91 !<General model parameter index
  INTEGER,PARAMETER :: m_fscmin    = 92 !<General model parameter index
  INTEGER,PARAMETER :: m_fsclim    = 93 !<General model parameter index
  INTEGER,PARAMETER :: m_fsck1     = 94 !<General model parameter index
  INTEGER,PARAMETER :: m_fsckexp   = 95 !<General model parameter index
!--Radiation estimation, and Potential Evapotranspiration parameters
  INTEGER,PARAMETER :: m_krs       = 96
  INTEGER,PARAMETER :: m_jhtadd    = 97
  INTEGER,PARAMETER :: m_jhtscale  = 98
  INTEGER,PARAMETER :: m_alfapt    = 99
  INTEGER,PARAMETER :: m_mwind     = 100
  INTEGER,PARAMETER :: m_grat3     = 101 !<General model parameter index 
!--Glacier parameters
  INTEGER,PARAMETER :: m_glacvcoef = 102
  INTEGER,PARAMETER :: m_glacvexp  = 103
  INTEGER,PARAMETER :: m_glacdens  = 104
  INTEGER,PARAMETER :: m_glacvcoef1 = 105
  INTEGER,PARAMETER :: m_glacvexp1  = 106
  INTEGER,PARAMETER :: m_glac2arlim = 107
!--Lake and river water temperature parameters, eventually replacing the t2trriver and t2trlake parameters
  INTEGER,PARAMETER :: m_tcfriver  = 108
  INTEGER,PARAMETER :: m_scfriver  = 109
  INTEGER,PARAMETER :: m_ccfriver  = 110
  INTEGER,PARAMETER :: m_lcfriver  = 111
  INTEGER,PARAMETER :: m_tcflake   = 112
  INTEGER,PARAMETER :: m_scflake   = 113
  INTEGER,PARAMETER :: m_ccflake   = 114
  INTEGER,PARAMETER :: m_lcflake   = 115
  INTEGER,PARAMETER :: m_stbcorr1  = 116
  INTEGER,PARAMETER :: m_stbcorr2  = 117
  INTEGER,PARAMETER :: m_stbcorr3  = 118
  INTEGER,PARAMETER :: m_zwind     = 119
  INTEGER,PARAMETER :: m_zwish     = 120
  INTEGER,PARAMETER :: m_zpdh      = 121
  INTEGER,PARAMETER :: m_roughness = 122
  INTEGER,PARAMETER :: m_pcelevstd = 123
  INTEGER,PARAMETER :: m_pcurain   = 124
  INTEGER,PARAMETER :: m_pcusnow   = 125
  INTEGER,PARAMETER :: m_cmrefr    = 126 
  INTEGER,PARAMETER :: m_fsceff    = 127
  INTEGER,PARAMETER :: m_glacalb   = 128           !glacier ice albedo (0.35)
  INTEGER,PARAMETER :: m_glacttmp  = 129
  INTEGER,PARAMETER :: m_glaccmlt  = 130
  INTEGER,PARAMETER :: m_glaccmrad = 131
  INTEGER,PARAMETER :: m_glaccmrefr= 132
  INTEGER,PARAMETER :: m_fepotglac = 133

  INTEGER,PARAMETER :: m_kthrflood = 134 !<General model parameter index
  INTEGER,PARAMETER :: m_klowflood = 135 !<General model parameter index
  INTEGER,PARAMETER :: m_limsedon  = 136 !<General model parameter index
  INTEGER,PARAMETER :: m_limsedpp  = 137 !<General model parameter index
!--Optimization parameters
  INTEGER,PARAMETER :: m_opt1     = 138
  INTEGER,PARAMETER :: m_opt2     = 139
  INTEGER,PARAMETER :: m_opt3     = 140
  INTEGER,PARAMETER :: m_opt4     = 141
  INTEGER,PARAMETER :: m_opt5     = 142
  INTEGER,PARAMETER :: m_opt6     = 143
  INTEGER,PARAMETER :: m_opt7     = 144
  INTEGER,PARAMETER :: m_opt8     = 145
  INTEGER,PARAMETER :: m_optonoff = 146

  INTEGER,PARAMETER :: m_sdnsmax  = 147
  INTEGER,PARAMETER :: m_sdnsrate = 148
  INTEGER,PARAMETER :: m_sdnsradd = 149
  INTEGER,PARAMETER :: m_gt2mix   = 150
  INTEGER,PARAMETER :: m_limt2exch = 151
  INTEGER,PARAMETER :: m_hsatINsoil = 152
  INTEGER,PARAMETER :: m_hsatINwater = 153
  INTEGER,PARAMETER :: m_hsatTP   = 154
!--Tracer parameters
  INTEGER,PARAMETER :: m_init1sw   = 155
  INTEGER,PARAMETER :: m_t1expdec  = 156
  INTEGER,PARAMETER :: m_t1freuc   = 157
  INTEGER,PARAMETER :: m_t1rel     = 158
  INTEGER,PARAMETER :: m_t1sed     = 159
  INTEGER,PARAMETER :: m_t1sedexp  = 160
  INTEGER,PARAMETER :: m_gldepo    = 161
  INTEGER,PARAMETER :: m_gicatch   = 162
  INTEGER,PARAMETER :: m_glacannmb = 163
  INTEGER,PARAMETER :: m_erodslope = 164
  INTEGER,PARAMETER :: m_erodexp   = 165
  INTEGER,PARAMETER :: m_erodindex = 166
  INTEGER,PARAMETER :: m_sedss = 167
  INTEGER,PARAMETER :: m_sedae = 168
  INTEGER,PARAMETER :: m_limsedss = 169
  INTEGER,PARAMETER :: m_wetspload  = 170
!--Fractional River Area Parameters
  INTEGER,PARAMETER :: m_fraxe      = 171
  INTEGER,PARAMETER :: m_fraxm      = 172
  INTEGER,PARAMETER :: m_ppenrstab  = 173
  INTEGER,PARAMETER :: m_ppenrflow  = 174
!--Wetland parameters
  INTEGER,PARAMETER :: m_wetrate  = 175
  INTEGER,PARAMETER :: m_iwetw0   = 176
  INTEGER,PARAMETER :: m_owetw0   = 177
  INTEGER,PARAMETER :: m_wetexp   = 178
  INTEGER,PARAMETER :: m_wlsed    = 179
  INTEGER,PARAMETER :: m_wlproddep  = 180
  INTEGER,PARAMETER :: m_wlmphuptin  = 181
  INTEGER,PARAMETER :: m_wlmphuptsp  = 182
  INTEGER,PARAMETER :: m_wlfastfrac  = 183
  INTEGER,PARAMETER :: m_wlpartfrac  = 184
  INTEGER,PARAMETER :: m_wltmpexp    = 185
!--snowfall distribution parameters  
  INTEGER,PARAMETER :: m_wsfscale = 186
  INTEGER,PARAMETER :: m_wsfbias  = 187
  INTEGER,PARAMETER :: m_sfdmax   = 188
  INTEGER,PARAMETER :: m_numdir   = 189
!--Hydraulic geometry of river
  INTEGER,PARAMETER :: m_hygeomf     = 190
  INTEGER,PARAMETER :: m_hygeomm     = 191
  INTEGER,PARAMETER :: m_hygeomc     = 192
  INTEGER,PARAMETER :: m_hygeomk     = 193
!--Macrophyte uptake
  INTEGER,PARAMETER :: m_muptn   = 194
  INTEGER,PARAMETER :: m_muptp   = 195
  INTEGER,PARAMETER :: m_muptdep = 196
!--lake and river ice radiation melt, breakup porosity, and heat flow water parameters
  INTEGER,PARAMETER :: m_licessmft  = 197 !<Parameter variable index 
  INTEGER,PARAMETER :: m_licessmfr  = 198 !<Parameter variable index 
  INTEGER,PARAMETER :: m_licermelt  = 199 !<Parameter variable index 
  INTEGER,PARAMETER :: m_licebupo   = 200 !<Parameter variable index
  INTEGER,PARAMETER :: m_liceqhw    = 201 !<Parameter variable index
  INTEGER,PARAMETER :: m_ricessmft  = 202 !<Parameter variable index 
  INTEGER,PARAMETER :: m_ricessmfr  = 203 !<Parameter variable index 
  INTEGER,PARAMETER :: m_ricermelt  = 204 !<Parameter variable index 
  INTEGER,PARAMETER :: m_ricebupo   = 205 !<Parameter variable index
  INTEGER,PARAMETER :: m_riceqhmn   = 206 !<Parameter variable index
  INTEGER,PARAMETER :: m_riceqhmx   = 207 !<Parameter variable index
  INTEGER,PARAMETER :: m_ricecwi    = 208 !<Parameter variable index
!--water holding capacity in snow
  INTEGER,PARAMETER :: m_whcsnow    = 209 !<Parameter variable index
  INTEGER,PARAMETER :: m_ricethpo   = 210 !<Parameter variable index
  !Travel time soil substance model
  INTEGER,PARAMETER :: m_inrel  = 211
  INTEGER,PARAMETER :: m_onrel  = 212
  INTEGER,PARAMETER :: m_sprel  = 213
  INTEGER,PARAMETER :: m_pprel  = 214
  INTEGER,PARAMETER :: m_ocrel  = 215
  INTEGER,PARAMETER :: m_ssrel  = 216
  INTEGER,PARAMETER :: m_aerel  = 217
  INTEGER,PARAMETER :: m_totexp1  = 218
  INTEGER,PARAMETER :: m_totexp2  = 219
  INTEGER,PARAMETER :: m_totexp3  = 220
  INTEGER,PARAMETER :: m_indecay  = 221
  INTEGER,PARAMETER :: m_ondecay  = 222
  INTEGER,PARAMETER :: m_spdecay  = 223
  INTEGER,PARAMETER :: m_ppdecay  = 224
  INTEGER,PARAMETER :: m_ocdecay  = 225
  INTEGER,PARAMETER :: m_ssdecay  = 226
  INTEGER,PARAMETER :: m_aedecay  = 227
  INTEGER,PARAMETER :: m_t1decay  = 228
  INTEGER,PARAMETER :: m_totexp0  = 229
  INTEGER,PARAMETER :: m_sloadinf1   = 230
  INTEGER,PARAMETER :: m_sloadinf2   = 231
  INTEGER,PARAMETER :: m_sloadinf3   = 232
  INTEGER,PARAMETER :: m_sloadonf1   = 233
  INTEGER,PARAMETER :: m_sloadonf2   = 234
  INTEGER,PARAMETER :: m_sloadonf3   = 235
  INTEGER,PARAMETER :: m_sloadspf1   = 236
  INTEGER,PARAMETER :: m_sloadspf2   = 237
  INTEGER,PARAMETER :: m_sloadspf3   = 238
  INTEGER,PARAMETER :: m_sloadppf1   = 239
  INTEGER,PARAMETER :: m_sloadppf2   = 240
  INTEGER,PARAMETER :: m_sloadppf3   = 241
  INTEGER,PARAMETER :: m_sloadocf1   = 242
  INTEGER,PARAMETER :: m_sloadocf2   = 243
  INTEGER,PARAMETER :: m_sloadocf3   = 244
  INTEGER,PARAMETER :: m_sloadssf1   = 245
  INTEGER,PARAMETER :: m_sloadssf2   = 246
  INTEGER,PARAMETER :: m_sloadssf3   = 247
  INTEGER,PARAMETER :: m_sloadaef1   = 248
  INTEGER,PARAMETER :: m_sloadaef2   = 249
  INTEGER,PARAMETER :: m_sloadaef3   = 250
  INTEGER,PARAMETER :: m_sloadt1f1   = 251
  INTEGER,PARAMETER :: m_sloadt1f2   = 252
  INTEGER,PARAMETER :: m_sloadt1f3   = 253
  INTEGER,PARAMETER :: m_totref      = 254
  INTEGER,PARAMETER :: m_muptnriver   = 255
  INTEGER,PARAMETER :: m_muptpriver   = 256
  INTEGER,PARAMETER :: m_muptdepriver = 257
!--Surface runoff additional parameter
  INTEGER,PARAMETER :: m_srbeta    = 258
  INTEGER,PARAMETER :: m_sralpha   = 259
  INTEGER,PARAMETER :: m_srgamma   = 260
  INTEGER,PARAMETER :: m_srnlayer  = 261
  INTEGER,PARAMETER :: m_spdecaq   = 262 !<General model parameter index
  INTEGER,PARAMETER :: m_ssdecaq   = 263 !<General model parameter index
  INTEGER,PARAMETER :: m_aedecaq   = 264 !<General model parameter index
  INTEGER,PARAMETER :: m_ppdecaq   = 265 !<General model parameter index
  INTEGER,PARAMETER :: m_t1decaq   = 266 !<General model parameter index
  INTEGER,PARAMETER :: m_ondecaq   = 267 !<General model parameter index
  INTEGER,PARAMETER :: m_ocdecaq   = 268 !<General model parameter index
  INTEGER,PARAMETER :: m_ricew0por = 269 !<Parameter variable index
  INTEGER,PARAMETER :: m_ricew1por = 270 !<Parameter variable index
  INTEGER,PARAMETER :: m_ricew0ice = 271 !<Parameter variable index
  INTEGER,PARAMETER :: m_ricew1ice = 272 !<Parameter variable index
!>\}

  !----basin parameters, none

!> \name Soil type dependent model parameter indices
!> \{
  INTEGER,PARAMETER :: m_wcfc     = 1  !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_wcwp     = 2  !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_wcep     = 3  !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_wcfc1    = 4  !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_wcwp1    = 5  !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_wcep1    = 6  !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_wcfc2    = 7  !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_wcwp2    = 8  !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_wcep2    = 9  !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_wcfc3    = 10 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_wcwp3    = 11 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_wcep3    = 12 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_srrate   = 13 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_trrcs    = 14 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_freuc    = 16 !<Soil type dependent model parameter index   
  INTEGER,PARAMETER :: m_freuexp  = 17 !<Soil type dependent model parameter index  
  INTEGER,PARAMETER :: m_freurate = 18 !<Soil type dependent model parameter index   
  INTEGER,PARAMETER :: m_rrcs1    = 19 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_rrcs2    = 20 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_macrate  = 21 !<Soil type dependent model parameter index  
  INTEGER,PARAMETER :: m_mactrinf = 22 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_mactrsm  = 23 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_soilcoh  = 24 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_soilerod = 25 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_sfrost   = 26 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_macfilt  = 27 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_perc1    = 28 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_perc2    = 29 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_rcgrwst  = 30 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_t1leakst = 31 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_bfroznsoil = 32
  INTEGER,PARAMETER :: m_erodsoil   = 33
  INTEGER,PARAMETER :: m_ppenrmax   = 34
  INTEGER,PARAMETER :: m_logsatmp   = 35 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_bcosby     = 36 !<Soil type dependent model parameter index 
  INTEGER,PARAMETER :: m_fzsexpand  = 37
  INTEGER,PARAMETER :: m_macfrac    = 38
!  INTEGER,PARAMETER :: m_srppor     = 38 !<Soil type dependent model parameter index
!>\}

!> \name Land use dependent model parameter indices
!> \{
  INTEGER,PARAMETER :: m_ttmp       = 1  !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_cmlt       = 2  !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_cevp       = 3  !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_cfrost     = 4  !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_srrcs      = 5  !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_humusN0    = 6  !<Land use dependent model parameter index   
  INTEGER,PARAMETER :: m_partP0     = 7  !<Land use dependent model parameter index   
  INTEGER,PARAMETER :: m_depthrel   = 8  !<Land use dependent model parameter index   
  INTEGER,PARAMETER :: m_surfmem    = 9  !<Land use dependent model parameter index   
  INTEGER,PARAMETER :: m_hNhalf     = 10 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_pPhalf     = 11 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_drypp      = 12 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_humusP0    = 13 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_hPhalf     = 14 !<Land use dependent model parameter index   
  INTEGER,PARAMETER :: m_denitrlu   = 15 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_ponatm     = 16 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_partP1     = 17 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_partP2     = 18 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_partP3     = 19 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_pcluse     = 20 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_minerfN    = 21 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_minerfP    = 22 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_degradhN   = 23 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_ripz       = 24 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_degradhP   = 25 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_filtPbuf   = 26 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_filtPinner = 27 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_filtPother = 28 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_humusC1    = 29 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_humusC2    = 30 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_humusC3    = 31 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_fastC1     = 32 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_fastC2     = 33 !<Land use dependent model parameter index  
  INTEGER,PARAMETER :: m_fastC3     = 34 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_dissolfN   = 35 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_dissolfP   = 36 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_dissolhN   = 37 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_dissolhP   = 38 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_ocsoim     = 39 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_ocsmslp    = 40 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_onconc0    = 41 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_ppconc0    = 42 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_occonc0    = 43 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_onpercred  = 44 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_pppercred  = 45 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_snalbmin   = 46 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_snalbmax   = 47 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_snalbkexp  = 48 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_cmrad      = 49 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_fscdistmax = 50 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_fscdist0   = 51 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_fscdist1   = 52 !<Land use dependent model parameter index
  INTEGER,PARAMETER,DIMENSION(5) :: m_kc = (/53,62,63,64,65/) !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_alb        = 54 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_fepotsnow  = 55 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_denitr3    = 56 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_t1leaklu   = 57 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_soilstretch = 58 !<Land use dependent model parameter index 
  INTEGER,PARAMETER :: m_ttrig      = 59 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_tredA      = 60 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_tredB      = 61 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_erodluse   = 66 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_wsfluse    = 67 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_snkika     = 68 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_intot      = 69 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_ontot      = 70 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_sptot      = 71 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_pptot      = 72 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_octot      = 73 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_sstot      = 74 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_aetot      = 75 !<Land use dependent model parameter index
  INTEGER,PARAMETER :: m_t1tot      = 76 !<Land use dependent model parameter index
!>\}

!> \name Parameter region dependent model parameter indices
!> \{
  INTEGER,PARAMETER :: m_cevpcorr   = 1  !<Parameter region parameter index
  INTEGER,PARAMETER :: m_rrcscorr   = 2  !<Parameter region parameter index
  INTEGER,PARAMETER :: m_ratcorr    = 3  !<Parameter region parameter index
  INTEGER,PARAMETER :: m_pirrs      = 4  !<Parameter region parameter index 
  INTEGER,PARAMETER :: m_preccorr   = 5  !<Parameter region parameter index
  INTEGER,PARAMETER :: m_cirrsink   = 6  !<Parameter region parameter index
  INTEGER,PARAMETER :: m_tcadd      = 7  !<Parameter region parameter index
  INTEGER,PARAMETER :: m_pirrg      = 8  !<Parameter region parameter index
  INTEGER,PARAMETER :: m_aqretcorr  = 9  !<Aquifer parameter region parameter index
  INTEGER,PARAMETER :: m_aqdelcorr  = 10 !<Aquifer parameter region parameter index
  INTEGER,PARAMETER :: m_aqpercorr  = 11 !<Parameter region parameter index
  INTEGER,PARAMETER :: m_cmltcorr   = 12 !<Parameter region parameter index
!>\}

!> \name Water quality parameter region dependent model parameter indices
!> \{
  INTEGER,PARAMETER :: m_incorr     = 13  !<Water quality parameter region parameter index  
  INTEGER,PARAMETER :: m_oncorr     = 14  !<Water quality parameter region parameter index  
  INTEGER,PARAMETER :: m_phoscorr   = 15  !<Water quality parameter region parameter index
  INTEGER,PARAMETER :: m_denit3reg  = 16  !<Water quality parameter region parameter index
!>\}

!> \name Ilake region dependent model parameter indices
!> \{
  INTEGER,PARAMETER :: m_ilrrat1     = 17
  INTEGER,PARAMETER :: m_ilrrat2     = 18
  INTEGER,PARAMETER :: m_ilrldep     = 19
  INTEGER,PARAMETER :: m_ilricatch   = 20
  INTEGER,PARAMETER :: m_ilhgdmdep   = 21
!>\}

!> \name Olake region dependent model parameter indices
!> \{
  INTEGER,PARAMETER :: m_olrrat1  = 22
  INTEGER,PARAMETER :: m_olrrat2  = 23
  INTEGER,PARAMETER :: m_olrldep  = 24
!>\}

!> \name Surface water (lake) region dependent model parameter indices (some only for olakes)
!> \{
  INTEGER,PARAMETER :: m_velpar1  = 25
  INTEGER,PARAMETER :: m_velpar2  = 26
  INTEGER,PARAMETER :: m_velpar3  = 27
  INTEGER,PARAMETER :: m_widpar1  = 28
  INTEGER,PARAMETER :: m_widpar2  = 29
  INTEGER,PARAMETER :: m_widpar3  = 30
  INTEGER,PARAMETER :: m_tpmean   = 31
  INTEGER,PARAMETER :: m_tnmean   = 32
  INTEGER,PARAMETER :: m_tocmean  = 33
!>\}

!> \name LakeData dependent model parameter indices
!> \{
  INTEGER,PARAMETER :: m_lddenitwl  = 1  !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldqmean    = 2  !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldtpmean   = 3  !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldtnmean   = 4  !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldtocmean  = 5  !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldwprodn   = 6  !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldwprodp   = 7  !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldwprodc   = 8  !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldsedon    = 9  !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldsedoc    = 10 !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldsedpp    = 11 !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldprodpp   = 12 !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldprodsp   = 13 !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldt2mix    = 14 !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldmuptN    = 17 !<LakeData parameter index
  INTEGER,PARAMETER :: m_ldmuptP    = 18 !<LakeData parameter index
!>\}

!> \name Monthly dependent model parameter indices
!> \{
  INTEGER,PARAMETER :: m_mlapse  = 1
!>\}

!> \name Glacier related constants
!> \{
  !Soil models, starting on 3 (0=default,1=olake,2=ilake)
  INTEGER, PARAMETER :: glacier_model    = 3   !<Glacier soil model identifier
!>\}

!> \name Nitrogen, phosphorus and organic carbon process constants
!> \{
    REAL,PARAMETER :: bulkdensity     = 1300.   !<soil density (kg/m3)
    REAL,PARAMETER :: NPratio         = 1.0/7.2 !<NP ratio for production/mineralisation in water 
    REAL,PARAMETER :: dryNratio       = 20.     !<dry weight to N ratio of algae
    REAL,PARAMETER :: maxdenitriwater = 0.5     !<maximum part of IN pool that can be denitrified per timestep
    REAL,PARAMETER :: maxprodwater    = 0.5     !<maximum part of IN/SRP pool that can be used for production per timestep
    REAL,PARAMETER :: maxdegradwater  = 0.5     !<maximum part of ON/PP/OC pool that can be degraded per timestep
    REAL,PARAMETER :: Satact          = 0.6     !<parameter for soil moisture factor for degradation/transformation in soil
    REAL,PARAMETER :: Thetalow        = 8.      !<parameter for soil moisture factor for degradation/transformation in soil
    REAL,PARAMETER :: Thetaupp        = 12.     !<parameter for soil moisture factor for degradation/transformation in soil
    REAL,PARAMETER :: Thetapow        = 1.      !<parameter for soil moisture factor for degradation/transformation in soil
    REAL,PARAMETER :: smfdenitlim     = 0.7     !<parameter for soil moisture factor for denitrification in soil (-)
    REAL,PARAMETER :: smfdenitpow     = 2.5     !<parameter for soil moisture factor for denitrification in soil (-)
    REAL,PARAMETER :: NCratio         = 5.7     !<NC ratio for production/mineralisation in water (value from BIOLA)
!>\}

!>\name Static variables
!>
!>The static variables are calculated before model time simulation starts and holds their value during simulation. 
!>This includes some status variables, recalculation of model parameters into a form suitable for the model 
!>processes and other useful variables.
!>\{
  LOGICAL :: calcSWRAD                     !<flagging need of shortwave radiation (depending on snowmelt or PET function)
  LOGICAL :: calcVAPOUR                    !<flagging need of vapour pressures and net radiation (depending on PET function)
  LOGICAL :: calcWIND                      !<flagging need of wind speed data (for Penman-Monteith PET function)
  LOGICAL :: T1leakage                     !<T1 is having a parameter leakage source
  REAL, ALLOCATABLE :: epotdist(:,:)       !<relative distribution of potential evaporation between upper two soil layers (-) (soillayer,class)
  REAL, ALLOCATABLE :: transtime(:,:)      !<translation time in river (total)
  REAL, ALLOCATABLE :: ttpart(:,:)         !<translation time in river (part of time step)
  INTEGER, ALLOCATABLE :: ttstep(:,:)      !<translation time in river (whole time step)
  REAL, ALLOCATABLE :: riverrc(:,:)        !<recession parameter for river damping
  REAL, ALLOCATABLE :: riverlength(:,:)    !<river length [m] (rivertype,subbasin)
  REAL, ALLOCATABLE :: deadriver(:,:)      !<volume of dead water in rivers (m3) (rivertype,subbasin)
  REAL, ALLOCATABLE :: deadwidth(:,:)      !<river width based om deadvolume (m) (rivertype,subbasin)
  REAL, ALLOCATABLE :: ratingk(:,:)        !<k-parameter for rating curve (laketype,subbasin)
  REAL, ALLOCATABLE :: iwetnoninflow(:)    !<fraction of runoff passing by iwet (subbasin)
  REAL, ALLOCATABLE :: wpmm(:,:)           !<water holding parameter (maxsoillayers,nclass)
  REAL, ALLOCATABLE :: fcmm(:,:)           !<water holding parameter (maxsoillayers,nclass)
  REAL, ALLOCATABLE :: epmm(:,:)           !<water holding parameter (maxsoillayers,nclass)
  REAL, ALLOCATABLE :: pwmm(:,:)           !<water holding capacity, pore volume (maxsoillayers,nclass)
  REAL, ALLOCATABLE :: soilrc(:,:,:)       !<soil runoff recession coefficients (maxsoillayers,nclass,nsub)
  REAL, ALLOCATABLE :: soilmem(:,:)        !<soil temperature memory (maxsoillayers,nclass)
  REAL, ALLOCATABLE :: basinrrcscorr(:)    !<subbasin rrcs correction (subbasin)
  REAL, ALLOCATABLE :: basincevpcorr(:)    !<subbasin evaporation correction (subbasin)
  REAL, ALLOCATABLE :: basincevpam(:)      !<subbasin evaporation correction (subbasin)
  REAL, ALLOCATABLE :: basincevpph(:)      !<subbasin evaporation correction (subbasin)
  REAL, ALLOCATABLE :: basinlp(:)          !<subbasin evaporation correction (subbasin)
  REAL, ALLOCATABLE :: basintcalt(:)       !<subbasin temperature correction (subbasin)
  REAL, ALLOCATABLE :: basintempadd(:)     !<subbasin temperature correction (subbasin)
  REAL, ALLOCATABLE :: basinpreccorr(:,:)  !<subbasin precipitation correction (subbasin,nclass)
  REAL, ALLOCATABLE :: basinpcurain(:)     !<subbasin rain correction (subbasin)
  REAL, ALLOCATABLE :: basinpcusnow(:)     !<subbasin snow correction (subbasin)
  REAL, ALLOCATABLE :: windtrans(:)        !<Wind transformation factor (nclass)
  REAL :: avertemp(4)                      !<number of timesteps over which meantemp is calculated
!> \brief Type for outflow for lakes with two outflows
  TYPE OUTFLOWTYPE
    INTEGER :: otype(2) = 0 !<type of outflow for two outlet lakes (depending on LakeData-parameters)
    INTEGER :: change = 0   !<flag for method to change outlet flows after updating total flow (depent on otype)
  END TYPE OUTFLOWTYPE
  TYPE(OUTFLOWTYPE), ALLOCATABLE :: lakeoutlet(:) !<information of lake outlet type (nbranch)
!>\}

!>\name Variables for river flows
!>
!>Some variables to help with river flow calculations. The variables hold values for each subbasin 
!>and often for both local and main flow. The variables for maximum daily flow during the last 
!>year are relating to the flow values saved in the state variable riverQ365.
!>\{
  !Model variables for river
  REAL, ALLOCATABLE :: Qmax(:,:),Q2max(:,:)      !<max Q and 2nd highest Q (=bankfull flow), from riverQ365 (rivertype,subbasin)
  INTEGER, ALLOCATABLE :: iQmax(:,:),iQ2max(:,:) !<index of max Q and 2nd highest Q in riverQ365 (rivertype,subbasin)
  !Variable for output
  REAL, ALLOCATABLE :: accdiff(:)           !<accumulated flow difference between simulated and observed flow, since last observed flow
!>\}

!>\name Load variables for source apportionment
!>
!>HYPE has variables to hold calculated nutrient loads. These variables contain the current time 
!>step load or transport of nitrogen and phosphorus for each source and subbasin. Two of the 
!>variables hold transport in different points in the river system (Lstream, Lpathway).
!>\{
  INTEGER, PARAMETER :: nsourload = 7            !< Number of sources of loads calculated for surface water
  INTEGER, PARAMETER :: npathway  = 19           !< Number of loads during transport calculated for surface water
  REAL,DIMENSION(:,:,:), ALLOCATABLE :: Latmdep  !< wet & dry atmospheric loads, (kg/timestep)
  REAL,DIMENSION(:,:,:), ALLOCATABLE :: Lcultiv  !< Cultivation loads, fertilizer and plant decay (kg/timestep)
  REAL,DIMENSION(:,:),   ALLOCATABLE :: Lirrsoil !< Class source load; irrigation on soil (kg/timestep)
  REAL,DIMENSION(:,:),   ALLOCATABLE :: Lrurala  !< Class source load; rural a (kg/timestep)
  REAL,DIMENSION(:),     ALLOCATABLE :: Lruralb  !< River load; rural b (kg/timestep)
  REAL,DIMENSION(:,:),   ALLOCATABLE :: Lpoints  !< Point Source Loads, urban/indust (kg/timestep)
  REAL,DIMENSION(:,:),   ALLOCATABLE :: Lstream  !< Loads by runoff to stream (soil, surface, tile), class dependent (kg/timestep)
  REAL,DIMENSION(:,:),   ALLOCATABLE :: Lpathway !< Loads of surface water from land to subbasin outlet (kg/timestep)
  REAL,DIMENSION(:),     ALLOCATABLE :: Lbranch  !< Load in branch (kg/timestep)
  REAL,DIMENSION(:,:),   ALLOCATABLE :: Lgrwsoil !< Soil loads from regional groundwater flow (kg/timestep)
  REAL,DIMENSION(:),     ALLOCATABLE :: Lgrwmr   !< Main river loads from regional groundwater (kg/timestep)
  REAL,DIMENSION(:),     ALLOCATABLE :: Lgrwol   !< Outlet lake loads from regional groundwater (kg/timestep)
  REAL,DIMENSION(:,:,:), ALLOCATABLE :: Lgrwclass !< Loads from soil by regional groundwater flow (kg/timestep) (not printed)
  REAL,DIMENSION(:),     ALLOCATABLE :: Ltransf  !< Water transfer load (kg/timestep)
!>\}

CONTAINS


  !>Initiate variables used for calculation of bankful flow
  !-----------------------------------------------------------
  SUBROUTINE set_Qvariables_for_bankfulflow(nsub,riverQ365)

    IMPLICIT NONE

    !Argument declarations
    INTEGER,INTENT(IN) :: nsub  !<number of subbasins
    REAL,INTENT(IN) :: riverQ365(366,2,nsub)  !<daily river flow last year

    !Local variables
    INTEGER i,j,iQ
    REAL riverQ365Temp(366)

      !Allocate 
      IF(.NOT.ALLOCATED(Qmax)) ALLOCATE(Qmax(2,nsub))   !!Qmax och Q2max beh�vs v�l inte sparas?
      IF(.NOT.ALLOCATED(Q2max)) ALLOCATE(Q2max(2,nsub))
      IF(.NOT.ALLOCATED(iQmax)) ALLOCATE(iQmax(2,nsub))
      IF(.NOT.ALLOCATED(iQ2max)) ALLOCATE(iQ2max(2,nsub))
     
      !Set parameter Qmax, iQmax, Q2max and iQ2max from riverstate%Q365
      DO i = 1,nsub
        DO j = 1,2
          riverQ365Temp = riverQ365(:,j,i)
          iQmax(j,i) = MAXLOC(riverQ365Temp,1)
          Qmax(j,i)  = riverQ365Temp(iQmax(j,i))
          riverQ365Temp(iQmax(j,i)) = 0
          iQ = MAXLOC(riverQ365Temp,1)
          riverQ365Temp(iQ) = 0              !!!????varf�r ta bort tv�, f� 3:e h�gsta???
          iQ2max(j,i) = MAXLOC(riverQ365Temp,1)
          Q2max(j,i)  = riverQ365Temp(iQ2max(j,i))
        ENDDO
      ENDDO

  END SUBROUTINE set_Qvariables_for_bankfulflow

END MODULE HYPEVARIABLES

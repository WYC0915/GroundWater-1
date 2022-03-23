/* ------------------------------------------------------------------------
* Bernese GPS Software Version 5.1
* -------------------------------------------------------------------------
*
* Name:       mainwin2.cpp
*
* Authors:
*
* Created:    __-___-____
*
* Changes:    12-Nov-2010 SL: STA2STA added to menuConv
*             15-Feb-2011 SL: STAMERGE added, new Service menu section
*             09-Dec-2011 SL: harmonization of the title strings (HLP,INP,$FG)
*             25-Sep-2012 SL/RD: add menu item README
*             15-Oct-2012 RD: V5.1 --> V5.2
*
* Copyright:  Astronomical Institute
*             University of Bern
*             Switzerland
* -----------------------------------------------------------------------*/

#ifndef WIN32
#include <unistd.h>
#endif

#include "mainwin.h"
#include "menu.h"
#include "initmenu.h"
#include "menutils.h"
#include "updpan.h"
#include "chngen.h"
#include "runpgm.h"
#include "errormsg.h"
#include "bpe.h"
#include "lockfile.h"
#include "myfildlg.h"
#include "r_file.h"

#ifdef USE_QWT
#include "graphwin.h"
#endif

// Initialize Popup Menus
////////////////////////////////////////////////////////////////////////////
void t_mainwin::initialize(const QString& inpFileName) {
  initialize();
  if ( !inpFileName.isEmpty() ) {
    rungpsFile = inpFileName;
    inpFiles.append(inpFileName);
    readAllInputs();
  }
}

void t_mainwin::initialize() {

  this->setCaption("Bernese GNSS Software Version 5.2");

  // Menu Bar and Popup Menus
  // ------------------------
  t_mymenu* menuLayout = new t_mymenu(this);
  menuLayout->insertItem("&Menu/statusbar font", MENU_ID_FONT_BASE);
  menuLayout->insertItem("&Panel text font"    , MENU_ID_FONT_LARGE);
  menuLayout->insertItem("&Browser text font"  , MENU_ID_FONT_SMALL);
  menuLayout->insertSeparator();
  menuLayout->insertItem("Select back&ground color", MENU_ID_BCKGRD_COLOR);
  menuLayout->insertItem("Select button &color",     MENU_ID_BUTTON_COLOR);
  menuLayout->insertSeparator();
  menuLayout->insertItem("&Reset menu layout"  , MENU_ID_FONT_RESET);

  t_mymenu* menuConfig = new t_mymenu(this);
  menuConfig->insertItem("Set session/compute &date", MENU_ID_JULDAT);
  menuConfig->insertItem("Menu &variables"          , MENU_ID_VAR);
  menuConfig->insertItem("&Program names"           , MENU_ID_PROGRAMS);
  menuConfig->insertItem("Paths and &extensions"    , MENU_ID_PATHS);
  menuConfig->insertSeparator();
  menuConfig->insertItem("Change &general options"  , MENU_ID_CHNGEN);
  menuConfig->insertItem("&Update program input files", MENU_ID_UPDPAN);
  menuConfig->insertSeparator();
  menuConfig->insertItem("Menu &layout"             , menuLayout);
  menuConfig->insertSeparator();

  QAction* closeAct = new QAction("Quit    ", this);
  closeAct->setShortcut(Qt::CTRL+Qt::Key_Q);
  connect(closeAct, SIGNAL(triggered()), this, SLOT(close()));
  menuConfig->addAction(closeAct);

  // ----------------------------------------------------------------------

  t_mymenu* menuStation = new t_mymenu(this);
  menuStation->insertItem("Station &coordinates",       MENU_ID_EDITCRD);
  menuStation->insertItem("Station &velocities",        MENU_ID_EDITVEL);
  menuStation->insertItem("Station &eccentricities",    MENU_ID_EDITECC);
  menuStation->insertItem("Station &information file",  MENU_ID_EDSTACRX);
  menuStation->insertSeparator();
  menuStation->insertItem("Station selection &list",    MENU_ID_EDITFIX);
  menuStation->insertItem("Station &sigma file",        MENU_ID_EDITSIG);
  menuStation->insertItem("&Observation sigma factors", MENU_ID_EDITSOS);
  menuStation->insertSeparator();
  menuStation->insertItem("&Abbreviation table",        MENU_ID_ABBREV);
  menuStation->insertItem("&Baseline definition file",  MENU_ID_EDITBSL);
  menuStation->insertItem("Cluste&r definition file",   MENU_ID_EDITCLU);
  menuStation->insertItem("&Tectonic plate assignment", MENU_ID_EDITPLD);

  t_mymenu* menuCampaign = new t_mymenu(this);
  menuCampaign->insertItem("&Select active campaign", MENU_ID_SELCAMP);
  menuCampaign->insertItem("&Edit list of campaigns", MENU_ID_CAMPLIST);
  menuCampaign->insertItem("Create &new campaign"   , MENU_ID_NEWCAMP);
  menuCampaign->insertSeparator();
  menuCampaign->insertItem("Edit session &table"    , MENU_ID_SESSIONS);
  menuCampaign->insertItem("Edit station &files"    , menuStation);

  // ----------------------------------------------------------------------

  t_mymenu* menuFromRinex = new t_mymenu(this);
  menuFromRinex->insertItem("&Observation files",      MENU_ID_RXOBV3);
  menuFromRinex->insertItem("&Navigation files" ,      MENU_ID_RXNBV3);
  menuFromRinex->insertItem("&Meteo files"      ,      MENU_ID_RXMBV3);
  menuFromRinex->insertItem("Navigation files to &SP3",MENU_ID_RXNPRE);

  t_mymenu* menuToRinex = new t_mymenu(this);
  menuToRinex->insertItem("&Observation files",MENU_ID_BV3RXO);
  menuToRinex->insertItem("&Navigation files", MENU_ID_BV3RXN);

  t_mymenu* menuConcatRinex = new t_mymenu(this);
  menuConcatRinex->insertItem("&Observation files",         MENU_ID_CCRINEXO);
  menuConcatRinex->insertItem("Navigation files (&GPS)",    MENU_ID_CCRINEXN);
  menuConcatRinex->insertItem("Navigation files (GLONA&SS)",MENU_ID_CCRINEXG);

  t_mymenu* menuUtilsRinex = new t_mymenu(this);
  menuUtilsRinex->insertItem("Create observation &statistics", MENU_ID_RNXGRA);
  menuUtilsRinex->insertItem("&Clean/smooth observation files",MENU_ID_RNXSMT);
  menuUtilsRinex->insertItem("&Extract satellite clock",       MENU_ID_RNXCLK);

  t_mymenu* menuRINEX   = new t_mymenu(this);
  menuRINEX->insertItem("Import RINEX to &Bernese format",  menuFromRinex);
  menuRINEX->insertItem("Export &RINEX from Bernese format",menuToRinex);
  menuRINEX->insertItem("&Cut/concatenate RINEX files",     menuConcatRinex);
  menuRINEX->insertItem("RINEX &utilities",                 menuUtilsRinex);

  // ----------------------------------------------------------------------

  t_mymenu* menuBrdc = new t_mymenu(this);
  menuBrdc->insertItem("&Check broadcast orbits",  MENU_ID_BRDTST);
  menuBrdc->insertItem("Create &tabular orbits" ,  MENU_ID_BRDTAB);
  menuBrdc->insertItem("&Extract satellite clocks",MENU_ID_SATCLK);

  t_mymenu* menuOrbCmp = new t_mymenu(this);
  menuOrbCmp->insertItem("&Standard orbits",    MENU_ID_STDDIF);
  menuOrbCmp->insertItem("&Precise orbits",     MENU_ID_ORBCMP);
  menuOrbCmp->insertItem("Osculating &elements",MENU_ID_STDELE);

  t_mymenu* menuOrbEOP = new t_mymenu(this);
  menuOrbEOP->insertItem("Convert IERS to &Bernese Format",MENU_ID_POLUPD);
  menuOrbEOP->insertItem("&Concatenate IERS pole files",   MENU_ID_POLXTR);

  t_mymenu* menuOrb = new t_mymenu(this);
  menuOrb->insertItem("&Broadcast orbits",                    menuBrdc);
  menuOrb->insertItem("Create &tabular orbits",               MENU_ID_PRETAB);
  menuOrb->insertItem("Create/update &standard orbits",       MENU_ID_ORBGEN);
  menuOrb->insertItem("&Extract ORBGEN program output",       MENU_ID_DEFXTR);
  menuOrb->insertSeparator();
  menuOrb->insertItem("Convert standard to &precise orbits",  MENU_ID_STDPRE);
  menuOrb->insertItem("Set &accuracy codes in precise orbits",MENU_ID_PREWEI);
  menuOrb->insertItem("&Concatenate/merge precise orbit files",MENU_ID_CCPREORB);
  menuOrb->insertItem("Co&mpare orbits",                      menuOrbCmp);
  menuOrb->insertSeparator();
  menuOrb->insertItem("Handle EOP &files",                    menuOrbEOP);

  // ----------------------------------------------------------------------

  t_mymenu* menuExtract = new t_mymenu(this);
  menuExtract->insertItem("&Code-based clock synchronization",MENU_ID_CODXTR);
  menuExtract->insertItem("Phase &preprocessing",             MENU_ID_MPRXTR);
  menuExtract->insertItem("Parameter &estimation/stacking",   MENU_ID_GPSXTR);

  t_mymenu* menuProcess = new t_mymenu(this);
  menuProcess->insertItem("&Code-based clock synchronization",MENU_ID_CODSPP);
  menuProcess->insertItem("Create &baseline files",           MENU_ID_SNGDIF);
  menuProcess->insertItem("Phase &preprocessing",             MENU_ID_MAUPRP);
  menuProcess->insertSeparator();
  menuProcess->insertItem("Parameter &estimation",            MENU_ID_GPSEST);
  menuProcess->insertItem("Combine &normal equation systems", MENU_ID_ADDNEQ);
  menuProcess->insertSeparator();
  menuProcess->insertItem("Program &output extraction",       menuExtract);

  // ----------------------------------------------------------------------

  t_mymenu* menuServSta = new t_mymenu(this);
  menuServSta->insertItem("&Convert station information files",MENU_ID_STA2STA);
  menuServSta->insertItem("&Merge station information files",  MENU_ID_STAMERGE);
  menuServSta->insertItem("Extract information from &RINEX",   MENU_ID_RNX2STA);
  menuServSta->insertItem("Extract information from &SINEX",   MENU_ID_SNX2STA);

  t_mymenu* menuServObs = new t_mymenu(this);
  menuServObs->insertItem("&Edit observation header file", MENU_ID_OBSHEAD);
  menuServObs->insertItem("Browse &observation data files",MENU_ID_OBSFILE);
  menuServObs->insertItem("Change &header",                MENU_ID_CHGHED);
  menuServObs->insertItem("&Mark/delete observations",     MENU_ID_SATMRK);
  menuServObs->insertItem("&Split observation files",      MENU_ID_OBSSPL);
  menuServObs->insertItem("Create pseudo-&graphics",       MENU_ID_SATGRA);

  t_mymenu* menuServResid = new t_mymenu(this);
#ifdef USE_QWT
  menuServResid->insertItem("&Plot ADDNEQ output",        MENU_ID_ADDNEQPLT);
#endif
  menuServResid->insertItem("&Display residual file",     MENU_ID_REDISP);
  menuServResid->insertItem("Create residual &statistics",MENU_ID_RESRMS);

  t_mymenu* menuServCoord = new t_mymenu(this);
  menuServCoord->insertItem("&Analysis of time series",  MENU_ID_FODITS);
  menuServCoord->insertSeparator();
  menuServCoord->insertItem("&Helmert transformation",   MENU_ID_HELMERT);
  menuServCoord->insertItem("Coordinate &comparison",    MENU_ID_COMPAR);
  menuServCoord->insertItem("&Velocity comparison",      MENU_ID_VELDIF);
  menuServCoord->insertSeparator();
  menuServCoord->insertItem("Compute &NUVEL velocities", MENU_ID_NUVELO);
  menuServCoord->insertItem("E&xtrapolate coordinates",  MENU_ID_COOVEL);
  menuServCoord->insertItem("Coordinate &transformation",MENU_ID_COOSYS);
  menuServCoord->insertItem("Transform to &ETRS89",      MENU_ID_ETRS89);
  menuServCoord->insertSeparator();
  menuServCoord->insertItem("&Merge coordinate/velocity files",
                                                         MENU_ID_CRDMRG);
  menuServCoord->insertSeparator();
  menuServCoord->insertItem("&Extract atmospheric tidal loading coefficients",
                                                         MENU_ID_GRDS1S2);

  t_mymenu* menuServClk = new t_mymenu(this);
  menuServClk->insertItem("Combine/manipulate c&lock RINEX files",MENU_ID_CCRNXC);
  menuServClk->insertItem("Epoch-wise &clock interpolation",MENU_ID_CLKEST);

  t_mymenu* menuServIon = new t_mymenu(this);
  menuServIon->insertItem("Local &ionosphere model estimation",MENU_ID_IONEST);

  t_mymenu* menuServLeo = new t_mymenu(this);
  menuServLeo->insertItem("Prepare LEO au&xiliary information",       MENU_ID_LEOAUX);
  menuServLeo->insertItem("Convert &kinematic pos. to precise orbits",MENU_ID_KINPRE);

  t_mymenu* menuServSlr = new t_mymenu(this);
  menuServSlr->insertItem("Convert &IRV/PRE to state vectors",     MENU_ID_IRV2STV);
  menuServSlr->insertItem("Convert &CPF to precise orbit files",   MENU_ID_CPFSP3);
  menuServSlr->insertItem("Convert &precise orbits to CPF files",  MENU_ID_SP3CPF);
  menuServSlr->insertSeparator();
  menuServSlr->insertItem("Prepare &SLR data handling file",       MENU_ID_SNX2SLR);
  menuServSlr->insertSeparator();
  menuServSlr->insertItem("Convert &quick-look files to RINEX",    MENU_ID_QLRINEXO);
  menuServSlr->insertItem("Create quick-look &residual statistics",MENU_ID_QLRSUM);

  t_mymenu* menuServAuto = new t_mymenu(this);
  menuServAuto->insertItem("Select &baselines", MENU_ID_BASLST);
  menuServAuto->insertItem("Form &clusters",     MENU_ID_MKCLUS);
  menuServAuto->insertItem("Detect &misbehaving stations/satellites",
                                                MENU_ID_RESCHK);

  t_mymenu* menuServ = new t_mymenu(this);
  menuServ->insertItem("&Generate simulated observation data", MENU_ID_GPSSIM);
  menuServ->insertSeparator();
  menuServ->insertItem("S&tation information files",           menuServSta);
  menuServ->insertItem("Bernese &observation files",           menuServObs);
  menuServ->insertItem("&Residual files",                      menuServResid);
  menuServ->insertSeparator();
  menuServ->insertItem("&Coordinate tools",                    menuServCoord);
  menuServ->insertItem("&Clock tools",                         menuServClk);
  menuServ->insertItem("&Ionosphere tools",                    menuServIon);
  menuServ->insertItem("&Automated processing",                menuServAuto);
  menuServ->insertSeparator();
  menuServ->insertItem("&LEO utilities",                       menuServLeo);
  menuServ->insertItem("&SLR utilities",                       menuServSlr);
  menuServ->insertSeparator();
  menuServ->insertItem("Browse &program output",               MENU_ID_JOB);
  menuServ->insertItem("Browse &error message",                MENU_ID_ERRMSG);

  // ----------------------------------------------------------------------

  t_mymenu* menuConvObs = new t_mymenu(this);
  menuConvObs->insertItem("Binary to &ASCII",  MENU_ID_OBSFMT);
  menuConvObs->insertItem("ASCII to &binary",  MENU_ID_FMTOBS);

  t_mymenu* menuConvRes = new t_mymenu(this);
  menuConvRes->insertItem("Binary to &ASCII",  MENU_ID_RESFMT);
  menuConvRes->insertItem("ASCII to &binary",  MENU_ID_FMTRES);

  t_mymenu* menuConvOrb = new t_mymenu(this);
  menuConvOrb->insertItem("Binary to &ASCII",  MENU_ID_STDFMT);
  menuConvOrb->insertItem("ASCII to &binary",  MENU_ID_FMTSTD);

  t_mymenu* menuConv = new t_mymenu(this);
  menuConv->insertItem("SINEX to normal &equations",          MENU_ID_SNX2NQ0);
  menuConv->insertItem("Manipulate &troposphere SINEX files", MENU_ID_TROTRO);
  menuConv->insertItem("&ANTEX to Bernese format",            MENU_ID_ATX2PCV);
  menuConv->insertSeparator();
  menuConv->insertItem("&Observation files",                  menuConvObs);
  menuConv->insertItem("&Residual files",                     menuConvRes);
  menuConv->insertItem("Or&bit files",                        menuConvOrb);
  menuConv->insertItem("&Normal equations (binary/ASCII)",    MENU_ID_NEQ2ASC);

  // ----------------------------------------------------------------------

  t_mymenu* menuBPE = new t_mymenu(this);
  menuBPE->insertItem("Edit &process control file (PCF)",    MENU_ID_EDTPCF);
//  menuBPE->insertItem("Edit PCF (&new format)",              MENU_ID_EDTPCF_NEW);
  menuBPE->insertItem("Edit PCF program &input files",       MENU_ID_EDTBPE);
  menuBPE->insertItem("Edit single &menu/program input file",MENU_ID_EDTINP);
  menuBPE->insertSeparator();
  menuBPE->insertItem("Edit &CPU file",                      MENU_ID_EDTCPU);
  menuBPE->insertItem("&Reset CPU file",                     MENU_ID_RESETCPU);
  menuBPE->insertSeparator();
  menuBPE->insertItem("&Start BPE processing",               MENU_ID_RUNBPE);

  // ----------------------------------------------------------------------

  _menuUser = new t_mymenu(this);
  _menuUser->insertItem("Program 0&1 :  ", MENU_ID_USER01);
  _menuUser->insertItem("Program 0&2 :  ", MENU_ID_USER02);
  _menuUser->insertItem("Program 0&3 :  ", MENU_ID_USER03);
  _menuUser->insertItem("Program 0&4 :  ", MENU_ID_USER04);
  _menuUser->insertItem("Program 0&5 :  ", MENU_ID_USER05);
  _menuUser->insertItem("Program 0&6 :  ", MENU_ID_USER06);
  _menuUser->insertItem("Program 0&7 :  ", MENU_ID_USER07);
  _menuUser->insertItem("Program 0&8 :  ", MENU_ID_USER08);
  _menuUser->insertItem("Program 0&9 :  ", MENU_ID_USER09);
  _menuUser->insertItem("Program 1&0 :  ", MENU_ID_USER10);
  updateMenuUser();

  // ----------------------------------------------------------------------

  t_mymenu* menuHelp = new t_mymenu(this);
  menuHelp->insertItem("&General",         MENU_ID_HELP_GENERAL);
  menuHelp->insertItem("&Readme",          MENU_ID_HELP_README);
  menuHelp->insertItem("&Help on context", MENU_ID_HELP_CONTEXT);
  menuHelp->insertSeparator();
  menuHelp->insertItem("&About",           MENU_ID_HELP_ABOUT);

  // ----------------------------------------------------------------------

  QMenuBar* menubar = new QMenuBar(this);
  menubar->insertItem("Con&figure ", menuConfig  );
  menubar->insertItem(" &Campaign ", menuCampaign);
  menubar->insertItem("  &RINEX   ", menuRINEX   );
  menubar->insertItem("&Orbits/EOP", menuOrb     );
  menubar->insertItem("&Processing", menuProcess );
  menubar->insertItem(" &Service  ", menuServ    );
  menubar->insertItem("Con&version", menuConv    );
  menubar->insertItem("   &BPE    ", menuBPE     );
  menubar->insertItem("   &User   ", _menuUser   );
  menubar->insertSeparator();
  menubar->insertItem("   &Help   ", menuHelp    );

  setMenuBar(menubar);

  setStatusBarMessage();
}

// Menu Items Switch
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotMenu(int item) {

  if (item == MENU_ID_HELP_GENERAL || item == MENU_ID_HELP_README ||
      item == MENU_ID_HELP_CONTEXT || item == MENU_ID_HELP_ABOUT) {
    return slotHelp(item);
  }

  int oldSlotMenuItem = currentSlotMenuItem;
  currentSlotMenuItem = item;
  initmenu.readInput();
  inpFiles.clear();

  static bool first = true;
  static QVector<const char*> fileKeys(MAX_MENU_ID);
  if (first) {
    first = false;
    fileKeys[MENU_ID_NEWCAMP ] = "NEWCAMP_INP";
    fileKeys[MENU_ID_SNX2NQ0 ] = "SNX2NQ0_INP";
    fileKeys[MENU_ID_SNX2STA ] = "SNX2STA_INP";
    fileKeys[MENU_ID_TROTRO  ] = "TROTRO_INP";
    fileKeys[MENU_ID_BV3RXO  ] = "BV3RXO_INP";
    fileKeys[MENU_ID_BV3RXN  ] = "BV3RXN_INP";
    fileKeys[MENU_ID_ETRS89  ] = "ETRS89_INP";
    fileKeys[MENU_ID_ATX2PCV ] = "ATX2PCV_INP";
    fileKeys[MENU_ID_RXOBV3  ] = "RXOBV3_INP";
    fileKeys[MENU_ID_RXNBV3  ] = "RXNBV3_INP";
    fileKeys[MENU_ID_RXMBV3  ] = "RXMBV3_INP";
    fileKeys[MENU_ID_RNXGRA  ] = "RNXGRA_INP";
    fileKeys[MENU_ID_RXNPRE  ] = "RXNPRE_INP";
    fileKeys[MENU_ID_RNXSMT  ] = "RNXSMT_INP";
    fileKeys[MENU_ID_RNXCLK  ] = "RNXCLK_INP";
    fileKeys[MENU_ID_CCRNXC  ] = "CCRNXC_INP";
    fileKeys[MENU_ID_CLKEST  ] = "CLKEST_INP";
    fileKeys[MENU_ID_CCRINEXO] = "CCRINEXO_INP";
    fileKeys[MENU_ID_CCRINEXN] = "CCRINEXN_INP";
    fileKeys[MENU_ID_CCRINEXG] = "CCRINEXG_INP";
    fileKeys[MENU_ID_GPSSIM  ] = "GPSSIM_INP";
    fileKeys[MENU_ID_PRETAB  ] = "PRETAB_INP";
    fileKeys[MENU_ID_BRDTAB  ] = "BRDTAB_INP";
    fileKeys[MENU_ID_BRDTST  ] = "BRDTST_INP";
    fileKeys[MENU_ID_ORBGEN  ] = "ORBGEN_INP";
    fileKeys[MENU_ID_STDPRE  ] = "STDPRE_INP";
    fileKeys[MENU_ID_PREWEI  ] = "PREWEI_INP";
    fileKeys[MENU_ID_CCPREORB] = "CCPREORB_INP";
    fileKeys[MENU_ID_SATCLK  ] = "SATCLK_INP";
    fileKeys[MENU_ID_STDELE  ] = "STDELE_INP";
    fileKeys[MENU_ID_STDDIF  ] = "STDDIF_INP";
    fileKeys[MENU_ID_ORBCMP  ] = "ORBCMP_INP";
    fileKeys[MENU_ID_CODSPP  ] = "CODSPP_INP";
    fileKeys[MENU_ID_SNGDIF  ] = "SNGDIF_INP";
    fileKeys[MENU_ID_MAUPRP  ] = "MAUPRP_INP";
    fileKeys[MENU_ID_GPSEST  ] = "GPSEST_INP";
    fileKeys[MENU_ID_IONEST  ] = "IONEST_INP";
    fileKeys[MENU_ID_LEOAUX  ] = "LEOAUX_INP";
    fileKeys[MENU_ID_KINPRE  ] = "KINPRE_INP";
    fileKeys[MENU_ID_IRV2STV ] = "IRV2STV_INP";
    fileKeys[MENU_ID_CPFSP3  ] = "CPFSP3_INP";
    fileKeys[MENU_ID_SP3CPF  ] = "SP3CPF_INP";
    fileKeys[MENU_ID_SNX2SLR ] = "SNX2SLR_INP";
    fileKeys[MENU_ID_QLRINEXO] = "QLRINEXO_INP";
    fileKeys[MENU_ID_QLRSUM  ] = "QLRSUM_INP";
    fileKeys[MENU_ID_ADDNEQ  ] = "ADDNEQ2_INP";
    fileKeys[MENU_ID_UPDPAN  ] = "UPDPAN_INP";
    fileKeys[MENU_ID_CHNGEN  ] = "CHNGEN_INP";
    fileKeys[MENU_ID_RUNBPE  ] = "RUNBPE_INP";
    fileKeys[MENU_ID_REDISP  ] = "REDISP_INP";
    fileKeys[MENU_ID_OBSFMT  ] = "OBSFMT_INP";
    fileKeys[MENU_ID_FMTOBS  ] = "FMTOBS_INP";
    fileKeys[MENU_ID_STDFMT  ] = "STDFMT_INP";
    fileKeys[MENU_ID_FMTSTD  ] = "FMTSTD_INP";
    fileKeys[MENU_ID_NEQ2ASC ] = "NEQ2ASC_INP";
    fileKeys[MENU_ID_FMTRES  ] = "FMTRES_INP";
    fileKeys[MENU_ID_RESFMT  ] = "RESFMT_INP";
    fileKeys[MENU_ID_DEFXTR  ] = "DEFXTR_INP";
    fileKeys[MENU_ID_CODXTR  ] = "CODXTR_INP";
    fileKeys[MENU_ID_MPRXTR  ] = "MPRXTR_INP";
    fileKeys[MENU_ID_GPSXTR  ] = "GPSXTR_INP";
    fileKeys[MENU_ID_RESCHK  ] = "RESCHK_INP";
    fileKeys[MENU_ID_CRDMRG  ] = "CRDMRG_INP";
    fileKeys[MENU_ID_COOVEL  ] = "COOVEL_INP";
    fileKeys[MENU_ID_RESRMS  ] = "RESRMS_INP";
    fileKeys[MENU_ID_POLXTR  ] = "POLXTR_INP";
    fileKeys[MENU_ID_POLUPD  ] = "POLUPD_INP";
    fileKeys[MENU_ID_CHGHED  ] = "CHGHED_INP";
    fileKeys[MENU_ID_SATMRK  ] = "SATMRK_INP";
    fileKeys[MENU_ID_SATGRA  ] = "SATGRA_INP";
    fileKeys[MENU_ID_OBSSPL  ] = "OBSSPL_INP";
    fileKeys[MENU_ID_AMBCHK  ] = "AMBCHK_INP";
    fileKeys[MENU_ID_FODITS  ] = "FODITS_INP";
    fileKeys[MENU_ID_HELMERT ] = "HELMERT_INP";
    fileKeys[MENU_ID_COMPAR  ] = "COMPAR_INP";
    fileKeys[MENU_ID_VELDIF  ] = "VELDIF_INP";
    fileKeys[MENU_ID_COOSYS  ] = "COOSYS_INP";
    fileKeys[MENU_ID_NUVELO  ] = "NUVELO_INP";
    fileKeys[MENU_ID_GRDS1S2 ] = "GRDS1S2_INP";
    fileKeys[MENU_ID_BASLST  ] = "BASLST_INP";
    fileKeys[MENU_ID_MKCLUS  ] = "MKCLUS_INP";
    fileKeys[MENU_ID_RNX2STA ] = "RNX2STA_INP";
    fileKeys[MENU_ID_STA2STA ] = "STA2STA_INP";
    fileKeys[MENU_ID_STAMERGE] = "STAMERGE_INP";
    fileKeys[MENU_ID_USER01  ] = "USER1_INP";
    fileKeys[MENU_ID_USER02  ] = "USER2_INP";
    fileKeys[MENU_ID_USER03  ] = "USER3_INP";
    fileKeys[MENU_ID_USER04  ] = "USER4_INP";
    fileKeys[MENU_ID_USER05  ] = "USER5_INP";
    fileKeys[MENU_ID_USER06  ] = "USER6_INP";
    fileKeys[MENU_ID_USER07  ] = "USER7_INP";
    fileKeys[MENU_ID_USER08  ] = "USER8_INP";
    fileKeys[MENU_ID_USER09  ] = "USER9_INP";
    fileKeys[MENU_ID_USER10  ] = "USER10_INP";
  }

  if      (item == MENU_ID_PROGRAMS) {
    inpFiles.append(initmenu.getInpFileName("MENU_PGM_INP"));
    readAllInputs(false);
  }
  else if (item == MENU_ID_PATHS) {
    inpFiles.append(initmenu.getInpFileName("MENU_EXT_INP"));
    readAllInputs(false);
  }
  else if (item == MENU_ID_CAMPLIST) {
    inpFiles.append(initmenu.getInpFileName("MENU_CMP_INP"));
    if (lockFile(inpFiles.last(),10,0.1)) {
      readAllInputs(false);
    }
  }
  else if (item == MENU_ID_VAR) {
    inpFiles.append(initmenu.getInpFileName("MENU_VAR_INP"));
    readAllInputs(false);
  }
  else if (item == MENU_ID_FONT_BASE) {
    selectFont(item);
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_FONT_LARGE) {
    selectFont(item);
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_FONT_SMALL) {
    selectFont(item);
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_BCKGRD_COLOR) {
    selectColor(item);
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_BUTTON_COLOR) {
    selectColor(item);
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_FONT_RESET) {
    resetFonts();
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_SELCAMP) {
    selCamp();
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_SESSIONS) {
    editSessions();
  }
  else if (item == MENU_ID_EDITCRD) {
    editStationFile("CRD_EDIT", "DIR_CRD", "EXT_CRD", "EDITCRD_INP",
                    "MENUAUX_SCR", "PTH_SCR", "Edit Coordinates", "", "");
  }
  else if (item == MENU_ID_EDSTACRX) {
    editStationFile("STAX_EDIT", "DIR_STA", "EXT_STA", "EDSTACRX_INP",
            "MENUAUX_SCR", "PTH_SCR", "Station Info",
            "DIR_CRD", "EXT_CRD");
  }
  else if (item == MENU_ID_ABBREV) {
    editStationFile("ABB_EDIT", "DIR_ABB", "EXT_ABB", "ABBREV_INP",
                    "MENUAUX_SCR", "PTH_SCR",
          "Abbreviations (Open an existing file or Save into a new file",
                    "DIR_CRD", "EXT_CRD");
  }
  else if (item == MENU_ID_EDITECC) {
    editStationFile("ECC_EDIT", "DIR_ECC", "EXT_ECC", "EDITECC_INP",
            "MENUAUX_SCR", "PTH_SCR", "Eccentricities",
            "DIR_CRD", "EXT_CRD");
  }
  else if (item == MENU_ID_EDITVEL) {
    editStationFile("VEL_EDIT", "DIR_VEL", "EXT_VEL", "EDITVEL_INP",
            "MENUAUX_SCR", "PTH_SCR", "Edit Velocities",
            "DIR_CRD", "EXT_CRD");
  }
  else if (item == MENU_ID_EDITFIX) {
    editStationFile("FIX_EDIT", "DIR_FIX", "EXT_FIX", "EDITFIX_INP",
            "MENUAUX_SCR", "PTH_SCR", "Station Selection List",
            "DIR_CRD", "EXT_CRD");
  }
  else if (item == MENU_ID_EDITSIG) {
    editStationFile("SIG_EDIT", "DIR_SIG", "EXT_SIG", "EDITSIG_INP",
            "MENUAUX_SCR", "PTH_SCR", "Sigma File",
            "DIR_CRD", "EXT_CRD");
  }
  else if (item == MENU_ID_EDITCLU) {
    editStationFile("CLU_EDIT", "DIR_CLU", "EXT_CLU", "EDITCLU_INP",
            "MENUAUX_SCR", "PTH_SCR", "Cluster File",
            "DIR_CRD", "EXT_CRD");
  }
  else if (item == MENU_ID_EDITBSL) {
    editStationFile("BSL_EDIT", "DIR_BSL", "EXT_BSL", "EDITBSL_INP",
            "MENUAUX_SCR", "PTH_SCR", "Baseline list",
            "DIR_CRD", "EXT_CRD");
  }
  else if (item == MENU_ID_EDITSOS) {
    editStationFile("SOS_EDIT", "DIR_SOS", "EXT_SOS", "EDITSOS_INP",
            "MENUAUX_SCR", "PTH_SCR", "Observation sigma factors",
            "DIR_CRD", "EXT_CRD");
  }
  else if (item == MENU_ID_EDITPLD) {
    editStationFile("PLD_EDIT", "DIR_PLD", "EXT_PLD", "EDITPLD_INP",
            "MENUAUX_SCR", "PTH_SCR", "Tectonic plate assignment",
            "DIR_CRD", "EXT_CRD");
  }
  else if (item == MENU_ID_JULDAT) {
    showJuldat();
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_EDTPCF) {
    if (mayIcloseInputFile()) {
      inpFiles.append( convPCF() );
      readAllInputs(false);
    }
  }
  else if (item == MENU_ID_EDTPCF_NEW) {
    QString inpFilter = "*." + initmenu.getKeySel0("EXT_PCF") ;
    QString inpDir    = expandEnvVar( initmenu.getPath("PTH_PCF") );
    inpFiles          = t_myfildlg::getOpenFileNames(inpFilter,inpDir);
    if (inpFiles.count() > 0) {
      readAllInputs(false);
    }
    else {
      currentSlotMenuItem = oldSlotMenuItem;
    }
  }
  else if (item == MENU_ID_EDTCPU) {
    QString inpFilter = "*." + initmenu.getKeySel0("EXT_CPU") ;
    QString inpDir    = expandEnvVar( initmenu.getPath("PTH_CPU") );
    inpFiles          = t_myfildlg::getOpenFileNames(inpFilter,inpDir);
    if (inpFiles.count() > 0) {
      readAllInputs(false);
    }
    else {
      currentSlotMenuItem = oldSlotMenuItem;
    }
  }
  else if (item == MENU_ID_RESETCPU) {
    QString inpFilter   = "*." + initmenu.getKeySel0("EXT_CPU") ;
    QString inpDir      = expandEnvVar( initmenu.getPath("PTH_CPU") );
    QString cpuFileName = t_myfildlg::getOpenFileName(inpDir, inpFilter);
    if (!cpuFileName.isEmpty()) {
      t_cpufile cpufile(cpuFileName);
      if (cpufile.ok()) {
        cpufile.resetCPU();
        errormsg("CPU File " + cpufile.getName() + " has been reset");
      }
    }
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_EDTBPE) {
    if (edtBPE() == QDialog::Rejected) {
      currentSlotMenuItem = oldSlotMenuItem;
    }
  }
  else if (item == MENU_ID_EDTINP) {
    QString inpFilter = "*." + initmenu.getKeySel0("EXT_INP") ;
    QString inpDir    = expandEnvVar( initmenu.getPath("PTH_OPT") );
    inpFiles          = t_myfildlg::getOpenFileNames(inpFilter,inpDir);
    if (inpFiles.size() == 0) {
      currentSlotMenuItem = oldSlotMenuItem;
    }
    else {
      readAllInputs(false);
    }
  }
  else if (item == MENU_ID_OBSHEAD) {
    editFile("DIR_PSH", "EXT_PSH EXT_CSH EXT_PZH EXT_CZH ", "OBSHEAD");
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_OBSFILE) {
    editFile("DIR_PSH", "EXT_PSH EXT_CSH EXT_PZH EXT_CZH ", "OBSFILE");
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_ADDNEQPLT) {
    showPlot();
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_JOB) {
    QString filter = "*." + initmenu.getKeySel0("EXT_OUT") +
                     " *.[L,0-9][0-9][0-9];;*.*";
    QString dir = expandEnvVar( initmenu.getPath("DIR_OUT") );
    showOutput(filter, dir);
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item == MENU_ID_ERRMSG) {
    QString filter = "*." + initmenu.getKeySel0("EXT_ERR") + ";;*.*";
    QString dir = expandEnvVar( initmenu.getPath("PTH_ERR") );
    showErrMsg(filter, dir);
    currentSlotMenuItem = oldSlotMenuItem;
  }
  else if (item >= 0 && item < MAX_MENU_ID) {
    inpFiles.append(initmenu.getInpFileName(fileKeys[item]));
    readAllInputs(true);
  }
}

// Run the Program
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotRun() {

  if (inpfile) {
    QString inpFileName = inpfile->getName();
    if ( slotSavePanels() == 0) {
      return ;
    }

    switch( currentSlotMenuItem ) {
      case MENU_ID_NEWCAMP:
        newCamp(inpFileName);
      break;
      case MENU_ID_UPDPAN:
        QApplication::setOverrideCursor( Qt::waitCursor );
        updpan(inpFileName);
        QApplication::restoreOverrideCursor();
      break;
      case MENU_ID_CHNGEN:
        QApplication::setOverrideCursor( Qt::waitCursor );
        chngen(inpFileName);
        QApplication::restoreOverrideCursor();
      break;
      case MENU_ID_RUNBPE:
        runbpe(inpFileName);
      break;
      default:
      {
        runpgm(inpFileName, errFileName, canvas, true, true);
        if (! rungpsFile.isEmpty()) {
          inpFiles.clear();
          inpFiles.append(rungpsFile);
          readAllInputs();
        }
      }
      break;
    }
    if (!lastOutput.isEmpty()) {
      outputAction->setEnabled(true);
    }
    if (currentSlotMenuItem != -1) {
      rerunAction->setEnabled(true);
    }
  }
}

// Plot File (stemming from e.g. ADDNEQ2)
////////////////////////////////////////////////////////////////////////////
void t_mainwin::showPlot() {
#ifndef USE_QWT
  errormsg("This version of the menu program     \n"
           "was compiled without USE_QWT option. \n"
           "Plotting facilities are not active.    ");
#else
  const int STANAMLENGTH = 16;

  QString pltFilter = "*." + initmenu.getKeySel0("EXT_PLT") + ";;*.*";
  QString outDir    = expandEnvVar( initmenu.getPath("DIR_PLT") );

  QString pltFileName = t_myfildlg::getOpenFileName(outDir, pltFilter, this);

  if (pltFileName.isEmpty()) {
    return;
  }

  r_file inFile(pltFileName);
  if ( !inFile.open(QIODevice::ReadOnly | QIODevice::Text) ) {
    errormsg("Cannot open file " + pltFileName);
    return ;
  }

  QString     line;
  QString     oldStaName;
  QStringList list;

  // Read all the Station Names
  // --------------------------
  while ( !inFile.eof() ) {
    line = inFile.readLine().stripWhiteSpace();
    if (line.isEmpty()) continue;
    if (line.left(STANAMLENGTH).stripWhiteSpace() != oldStaName) {
      oldStaName = line.left(STANAMLENGTH);
      list.append(oldStaName);
    }
  }
  inFile.close();

  oldStaName = "";
  qHeapSort(list);
  for (int ii = 0; ii < (int) list.count(); ii++) {
    if (oldStaName != list[ii] ) {
      oldStaName = list[ii];

      inFile.open(QIODevice::ReadOnly | QIODevice::Text) ;

      QStringList hlpList;
      QString     hlpLine;

      while ( !inFile.eof() ) {
        hlpLine = inFile.readLine().stripWhiteSpace();
        if (hlpLine.isEmpty()) continue;
        if (hlpLine.left(STANAMLENGTH).stripWhiteSpace() ==
            oldStaName.stripWhiteSpace()) {
          hlpList.append(hlpLine);
        }
      }
      inFile.close();

      int nPoints = hlpList.count() / 3;

      // Allocate Variables
      // ------------------
      double* nRes = new double[nPoints];
      double* eRes = new double[nPoints];
      double* uRes = new double[nPoints];
      double* time = new double[nPoints];

      int iFile;
      int iCrd;

      for (int ii = 0; ii < (int) nPoints; ++ii) {
        QString hlp;
            hlp = hlpList[3*ii].mid(STANAMLENGTH+1);
        QTextStream in1(&hlp, QIODevice::ReadOnly);
        in1 >> iFile >> iCrd >> nRes[ii] ;

            hlp = hlpList[3*ii+1].mid(STANAMLENGTH+1);
        QTextStream in2(&hlp, QIODevice::ReadOnly);
        in2 >> iFile >> iCrd >> eRes[ii] ;

            hlp = hlpList[3*ii+2].mid(STANAMLENGTH+1);
        QTextStream in3(&hlp, QIODevice::ReadOnly);
        in3 >> iFile >> iCrd >> uRes[ii] >> time[ii];
      }

      double**  yy = new double* [3];
      yy[0] = nRes; yy[1] = eRes; yy[2] = uRes;

      QStringList legend;
      legend.append("north");
      legend.append("east");
      legend.append("up");

      t_graphwin graph(this, nPoints, 3, time, yy, oldStaName, legend,
                       "MJD", "m");
      graph.exec();

      delete[] time;
      delete[] nRes;
      delete[] eRes;
      delete[] uRes;
      delete[] yy;
    }
  }
#endif
}

// Message on Status Bar
////////////////////////////////////////////////////////////////////////////
void t_mainwin::setStatusBarMessage() {
  QString str1 = "$Y+0";
  QString str2 = "$S+0";

  initmenu.evalDollar(str1);
  initmenu.evalDollar(str2);

  QString msg = "> ";

#ifndef CELMECH
  msg += "User: " + expandEnvVar("${USER}  ");

  msg += QString().sprintf( "Campaign: %s   $Y+0=%s  $S+0=%s",
                            initmenu.getActiveCamp().ascii(),
                            str1.ascii(), str2.ascii() );
  QString jobID = initmenu.getKeySel0("JOB_ID");
  if (!jobID.isEmpty()) {
    msg += "  $J=" + jobID;
  }
#endif

  if (inpfile) {
    msg = msg + "   File: ";
    int maxWidth = statusBar()->width() -
                   QFontMetrics(this->font()).width(msg) - 20;
    QString name = inpfile->getName();
    for (int nChar = name.length(); nChar > 0; nChar--) {
      QString hlp = name.right(nChar);
      if (QFontMetrics(this->font()).width(hlp) < maxWidth) {
        msg = msg + hlp;
        break;
      }
    }
  }
  statusBar()->showMessage(msg);
}

// Update User-Defined Menu
////////////////////////////////////////////////////////////////////////////
void t_mainwin::updateMenuUser() {
  initmenu.readInput();
  for (int ii = 0; ii < (int) _menuUser->count(); ii++) {
    QString key; key.sprintf("USER%d_INP", ii+1);
    QString pgm = stripPath( initmenu.getInpFileName(key), true);
    QString txt; txt.sprintf("User Program %d&%d: %s", (ii+1)/10,
                                                   (ii+1)%10, pgm.ascii()) ;
    _menuUser->changeItem(_menuUser->idAt(ii), txt);
    if (pgm.isEmpty()) {
      _menuUser->setItemEnabled(_menuUser->idAt(ii), false);
    }
    else {
      _menuUser->setItemEnabled(_menuUser->idAt(ii), true);
    }
  }
}

// Show About Window
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotAbout() {

  QString    relFileName = initmenu.getKeySel0("RELEASE_INFO");
  QByteArray relInfo;
  if (!relFileName.isEmpty()) {
    r_file relFile(expandEnvVar(relFileName));
    if ( relFile.open(QIODevice::ReadOnly) ) {
      relInfo = relFile.readAll();
    }
  }

  errormsg("\nBernese GNSS Software   \n\n"+ relInfo +
           "\nAstronomical Institute \n"
           "University of Bern\n\n"
           "http://www.bernese.unibe.ch\n");
}

// Reset Fonts, Size, Colors
////////////////////////////////////////////////////////////////////////////
void t_mainwin::resetFonts() {
  if (QMessageBox::warning(0, "Question",
                 "Do you really want to reset the default appearance?",
                  QMessageBox::No | QMessageBox::Default,
                  QMessageBox::Yes ) != QMessageBox::Yes ) {
    return;
  }

  // Size
  // ----
  this->resize(DEFAULT_MAIN_WIDTH, DEFAULT_MAIN_HEIGHT);

  // Colors
  // ------
  QStringList rgb = QStringList::split(QRegExp("\\s+"), DEFAULT_MAIN_PALETTE);
  QApplication::setPalette(QPalette(
            QColor(rgb[0].toInt(), rgb[1].toInt(), rgb[2].toInt()),
            QColor(rgb[3].toInt(), rgb[4].toInt(), rgb[5].toInt())), true);
  initmenu.saveKey("MAIN_PALETTE", DEFAULT_MAIN_PALETTE);
  if (!inpfile) {
    newCanvas();;
  }

  // Fonts
  // -----
  QFont newFontBase = QFont(DEFAULT_FONTTYPE_BASE, DEFAULT_FONTSIZE_BASE);
  initmenu.setFontBase(newFontBase);
  QApplication::setFont(newFontBase, true);

  QFont newFontLarge = QFont(DEFAULT_FONTTYPE_LARGE, DEFAULT_FONTSIZE_LARGE);
  initmenu.setFontLarge(newFontLarge);
  canvas->setFont(newFontLarge);
  resetPanel();

  QFont newFontSmall = QFont(DEFAULT_FONTTYPE_SMALL, DEFAULT_FONTSIZE_SMALL);
  initmenu.setFontSmall(newFontSmall);
}

// t_mymenu class
////////////////////////////////////////////////////////////////////////////
void t_mymenu::insertItem(const QString& text, int id) {
  t_myaction* act = new t_myaction(text, _parent, id);
  addAction(act);
}
void t_mymenu::insertItem(const QString& text, QMenu* popup) {
  popup->setTitle(text);
  QMenu::addMenu(popup);
}

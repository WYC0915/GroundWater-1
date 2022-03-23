======================================================================
Bernese GNSS Software, Version 5.2              Last mod.: 06-Nov-2012
----------------------------------------------------------------------
Installation of the ocean tidal loading coefficients
======================================================================


----------------------------------------------------------------------
Ocean tide load displacements modelling
----------------------------------------------------------------------
On the web site of the free ocean tide loading provider

  http://www.oso.chalmers.se/~loading

the loading coefficents for a set of stations (coordinates) can be
generated and downloaded.

- Select the ocean tide model (e.g., FES2004)

- Consider "vertical and horizontal displacements"

- Following the IGS conventions you have to answer the question
  "Do you want to correct your loading values for the motion?"
  with "NO" (corrections refer to the "CE: center of solid Earth").
  In that case you need to copy the corresponding corrections (CMC
  parameters) for the centre of mass motion of the Earth due to the
  ocean tides to the header part of your BLQ file.
  (Follow the link on the web site to get the tabulated CMC values.)

- The Bernese GNSS Software expects the ocean tidal corrections in BLQ
  format.


----------------------------------------------------------------------
IGSMAIL-3521
----------------------------------------------------------------------
From hgs at oso.chalmers.se  Wed Sep 12 06:02:51 2001
From: hgs at oso.chalmers.se (Hans-Georg Scherneck)
Date: Wed, 12 Sep 2001 13:02:51 +0000
Subject: [IGSMAIL-3521]: Automatic ocean loading provider
Message-ID: <3B9F5CF8.48A89071@oso.chalmers.se>

******************************************************************************
IGS Electronic Mail      12 Sep 06:02:58 PDT 2001      Message Number 3521
******************************************************************************

Author: Scherneck/Bos

Dear colleague,
We have the pleasure to announce the availablilty of an automatic ocean loading
provider on the internet.

http://www.oso.chalmers.se/~loading

You can select between 11 different ocean models and between the components
vertical displacement, horizontal displacement, and gravity. The software uses
recursive regional refinement of coastlines. You get the loading parameters for
11 partial tide waves in the familiar blq-format. The software behind it draws
from existing programs at Chalmers/Onsala Space Observatory, but extends their
previous capabilities a substantial step further.

The creation of the service has been a collaborative effort between Machiel S.
Bos, now at Delft University of Technology, and Hans-Georg Scherneck,
Chalmers/Onsala Space Observatory.

Documents are in progress.

With best regards
yours

Hans-Georg Scherneck and Machiel Simon Bos
Onsala / Delft, Sep 12, 2001

--
                                                             -----
With best regards yours                                    /   A   \
Hans-Georg                                                |   / \   |
---------------------+----------------------------------   \       /
Telephone:           | Hans-Georg Scherneck                  -----
 +46 31 772 5556     | Onsala Space Observatory               | |
Telefax:             | Chalmers University of Technology     /   \
 +46 31 772 5590     | S-439 92  Onsala, Sweden         ----------------
E-Mail:              | ftp://gere.oso.chalmers.se/pub   /===\ ,===//===\
 hgs at oso.chalmers.se | http://www.oso.chalmers.se/~hgs//    /`===,/    /
---------------------+-------------------------------- \===//===' \===/


======================================================================

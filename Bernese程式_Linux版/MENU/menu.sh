#!/bin/sh
# ==============================================================================
#
# Name:       menu.sh
#
# Authors:
#
# Created:    __-___-____
#
# Changes:    19-Aug-2012 SL: Allow third argument w/ -copyMenu_new
#
# ==============================================================================

# Redefine QT-lib path
# --------------------
if [ "$QTBERN" != "" ]
then
  QTDIR="$QTBERN"
  export QTDIR
fi


# Run a copy of $XQ/menu, non-interactive mode
# --------------------------------------------
if [ "$3" = "-copyMenu" ]
then
  cp $XQ/menu $U/WORK/menu_$$
  $U/WORK/menu_$$ $1 $2
  ircode=$?
  rm $U/WORK/menu_$$

# Run a copy of $XQ/menu_new, non-interactive mode
# ------------------------------------------------
elif [ "$3" = "-copyMenu_new" ]
then
  echo "Running menu_new..."
  cp $XQ/menu_new $U/WORK/menu_$$
  $U/WORK/menu_$$ $1 $2
  ircode=$?
  rm $U/WORK/menu_$$

# Run a copy of $XQ/menu, interactive mode
# ----------------------------------------
elif [ "$2" = "-copyMenu" ]
then
  cp $XQ/menu $U/WORK/menu_$$
  $U/WORK/menu_$$ $1
  ircode=$?
  rm $U/WORK/menu_$$

# Run a copy of $XQ/menu_new, interactive mode
# --------------------------------------------
elif [ "$2" = "-copyMenu_new" ]
then
  if [ "$3" = "" ]; then
    echo "Running menu_new..."
    cp $XQ/menu_new $U/WORK/menu_$$
  else
    if [ -x "$XQ/$3" ]; then
      echo "Running \$XQ/$3..."
      cp $XQ/$3 $U/WORK/menu_$$
    else
      echo "Running $3..."
      cp $3 $U/WORK/menu_$$
    fi
  fi
  $U/WORK/menu_$$ $1
  ircode=$?
  rm $U/WORK/menu_$$

# Run a copy of $XQ/menu, for AIUB only
# -------------------------------------
elif [ "$GROUP" = "aiub" ]
then
  cp $XQ/menu $U/WORK/menu_$$
  $U/WORK/menu_$$ $1 $2
  ircode=$?
  rm $U/WORK/menu_$$

# Run $XQ/menu without copy
# -------------------------
else
  $XQ/menu $1 $2
  ircode=$?
fi

exit $ircode

# ==============================================================================

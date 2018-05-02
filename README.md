# xi-ctm
this is a small tool to detect inversion/rotate/scaling of touch device, based
on the output of `xinput_calibrator` tool.

It will set Coordinate Matrix transformation value in xinput and will output
xorg's config (as like `xinput_calibrator` behaviour)

The intended method to use:

```
  export XICTM_DEVICE="device"
  export DISPLAY=:0
  xinput set-prop $XICTM_DEVICE 'Coordinate Transformation Matrix' 1 0 0 0 1 0 0 0 1
  xinput_calibrator --fake -v --misclick 0 --no-timeout | \
    XICTM_DEVICE="device-name-from-xinput" xi-ctm
```

# Building and installing

```
  wget -qO- https://get.haskellstack.org/ | sh
  stack test
  ./install.sh
```

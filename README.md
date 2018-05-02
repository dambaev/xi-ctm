# xi-ctm
this is a small tool to detect inversion/rotate/scaling of touch device, based
on the output of `xinput_calibrator` tool.

It will set Coordinate Matrix transformation value in xinput and will output
xorg's config (as like `xinput_calibrator` behaviour)

The intended method to use:

```
  export 
  export DISPLAY=:0
  xinput_calibrator --fake -v --misclick 0 --no-timeout | \
    XICTM_DEVICE="device-name-from-xinput" xi-ctm
```

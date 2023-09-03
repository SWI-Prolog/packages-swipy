# XSB benchmarks

To run:

  - build the janus_swi Python package using `pip install .` in the `swipy`
    main directory.
  - Run `python jns_plg_benches.py`
  - Run `swipl -O -g bench -t halt jns_plg_benches.pl`

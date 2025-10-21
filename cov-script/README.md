# Coverage Script

This script processes a log from GREASE and produces an [EZCOV](https://github.com/nccgroup/Cartographer/blob/main/EZCOV.md) script that [Cartographer](https://github.com/nccgroup/Cartographer/tree/main)
can process. The script highlights instructions run by GREASE.

The script can be run with uv:
```
uv run python main.py --addrmap <address map> --output <ez cov file> <log file> <original binary>
```

The address map is in the format:
```
[]
    {
        "lower_bound": LOWER_BOUND,
        "upper_bound": UPPER_BOUND,
        "base": BASE_OFFSET
    } ...
]
```

The lower and upper bound describe a Macaw segment and the base describes the offset of that segment in ghidra's database. 
That is for address `ADDR` the script will apply the transformation `BASE_OFFSET+(ADDR - LOWER_BOUND)` for any address in the segment.
This mechanism handles mismatches between a Ghidra load offset and Macaw load offsets (in the future we would like to resolve this).
from pathlib import Path

for dir in Path(".").glob("grease*"):
    print(dir)
    if not dir.is_dir():
        continue
    for f in dir.rglob("*.hs"):
        # print(f)
        lines = f.read_text().splitlines()
        in_imports = False
        out_lines = []
        imports = []
        for line in lines:
            is_comment = line.startswith("--")
            is_haddock = line.startswith("-- |")
            is_import = line.startswith("import")
            in_imports |= is_import
            if not in_imports:
                out_lines.append(line)
                continue
            if is_comment and not is_haddock:
                continue
            if is_import:
                imports.append(line)
                continue
            if line == "":
                continue

            # we just processed the last import
            in_imports = False
            imports = sorted(imports)
            out_lines += imports
            out_lines.append("")
            out_lines.append(line)

        out_lines.append("")
        f.write_text("\n".join(out_lines))

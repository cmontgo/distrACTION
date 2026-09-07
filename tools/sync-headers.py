#!/usr/bin/env python3
"""Bring R/<dist>.h.R into line with jamovi/<dist>.a.yaml and .r.yaml.

jmvtools::prepare() is the authoritative generator, but it needs jmvcore, which
is not installable here. This does the mechanical part so the committed headers
stop contradicting the YAML - the geometric's minimum, the normal's SD minimum,
every binomial and hypergeometric constraint, and a clearWith block that names
options this module has never had.

Run it after editing any jamovi/*.a.yaml, then let a real prepare() overwrite it.
"""
import re, sys, pathlib, yaml

root = pathlib.Path(__file__).resolve().parent.parent
changed = []

for apath in sorted((root / "jamovi").glob("*.a.yaml")):
    base = apath.name[:-len(".a.yaml")]
    hpath = root / "R" / f"{base}.h.R"
    if not hpath.exists():
        sys.exit(f"missing header: {hpath}")

    spec = yaml.safe_load(apath.read_text())
    src = hpath.read_text()
    orig = src

    for opt in spec.get("options", []):
        name, otype = opt.get("name"), opt.get("type")
        if not name or otype not in ("Number", "Integer"):
            continue

        # Rebuild the constructor call for this option from the YAML.
        args = [f'                "{name}",', f"                {name},"]
        for key in ("min", "max"):
            if key in opt:
                v = opt[key]
                if isinstance(v, str):
                    # YAML 1.1 reads bare "1e-10" as a string. Coerce, so a
                    # quoting quirk cannot emit a character bound into R.
                    try:
                        v = float(v)
                    except ValueError:
                        sys.exit(f"{apath.name}: {name}.{key} is not numeric: {v!r}")
                args.append(f"                {key}={v!r},")
        if "default" in opt:
            args.append(f"                default={opt['default']})")
        else:
            args[-1] = args[-1].rstrip(",") + ")"
        block = (f"            private$..{name} <- jmvcore::Option{otype}$new(\n"
                 + "\n".join(args))

        pattern = re.compile(
            r"            private\$\.\.%s <- jmvcore::Option(?:Number|Integer)\$new\(.*?\)\n"
            % re.escape(name), re.S)
        if not pattern.search(src):
            sys.exit(f"{hpath.name}: no constructor found for option {name}")
        src = pattern.sub(block + "\n", src, count=1)

    # Drop clearWith entries naming options this module does not define. They are
    # leftovers from the t-test template the analyses were first copied from.
    known = {o["name"] for o in spec.get("options", []) if "name" in o}
    def strip_clearwith(m):
        names = re.findall(r'"([^"]+)"', m.group(1))
        return "" if names and not (set(names) & known) else m.group(0)
    src = re.sub(r"\n *clearWith=list\((.*?)\),", strip_clearwith, src, flags=re.S)

    if src != orig:
        hpath.write_text(src)
        changed.append(hpath.name)

print("synced:", ", ".join(changed) if changed else "nothing to do")

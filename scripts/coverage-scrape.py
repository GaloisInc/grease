#!/usr/bin/env python3

# Scrape just the HTML table from an HPC report.
#
# Requires beautifulsoup4, try `pip install beautifulsoup4==4.13.5`.

from argparse import ArgumentParser
from bs4 import BeautifulSoup
from pathlib import Path


def go(path: Path, /):
    html = path.read_text()
    soup = BeautifulSoup(html, "html.parser")
    table = soup.find("table", class_="dashboard")
    print(table)


parser = ArgumentParser()
parser.add_argument("path", type=Path)
args = parser.parse_args()
go(args.path)

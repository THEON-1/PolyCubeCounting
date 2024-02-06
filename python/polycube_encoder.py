from json import JSONEncoder
from typing import Any
from polycube import PolyCube

class PolyCubeEncdoder(JSONEncoder):
    def default(self, o: Any) -> Any:
        if isinstance(o, PolyCube):
            return list(o.cubes)
        elif isinstance(o, set):
            return list(o)
        return super().default(o)

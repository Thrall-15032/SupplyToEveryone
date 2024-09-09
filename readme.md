Mode for HOI-4:

100% supply everywhere, specifically:
- Infrastructure 5 level in each state;
- Supply node in each province;
- Naval base 10 level in each coastal province;
- Train way 5 level between each neighboring province.

Converter:

**Button1** - reading files from '\history\states\' folder and add "buildings" chapter like that

    buildings={
        infrastructure=5
            3380={
                supply_node=1
                naval_base=10
            }
    }

That new "buildings" chapter (which in end of "history" chapter) upgrade original "buildings" chapter. I mean make infrastructure=5, add supply nodes and naval bases, but remain factories and bunkers.

**Button2** - generate neighbor.txt file with neighboring provinces ids like that

    <ProvinceId> <NeighProvinceId1> <NeighProvinceId2> <NeighProvinceIdN>

and railways.txt like that

    5 2 <ProvinceId1> <ProvinceId2>


**Button3** - button for quick debug testing.
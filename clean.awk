#!/usr/bin/env awk

FS="\t"
OFS="\t"
{
    if (($6 >= 1960) && ($8 ~ "[0-9]+"))
    {
        print $1, $4, $6, $8, $9
    }
}

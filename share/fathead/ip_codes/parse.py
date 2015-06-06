from itertools import product
import csv

particles = {
    "0": "None",
    "1": "Back of hand",
    "2": "Fingers",
    "3": "Tools",
    "4": "Wire",
    "5": "Dust protected",
    "6": "Dust tight",
    "X": "Unspecified"
}

water = {
    "0": "None",
    "1": "Dripping water",
    "2": "Dripping water, object tilted",
    "3": "Spraying water",
    "4": "Splashing water",
    "5": "Water jets",
    "6": "Powerful water jets",
    "6K": "Powerful water jets, high pressure",
    "7": "Immersion to 1 m",
    "8": "Immersion greater than 1 m",
    "9K": "Powerful water jets, high temperature, high pressure",
    "X": "Unspecified"
}

def article(name, desc):
    return [name, 'A', '', '', '', '', '', '', '', '', '', desc, 'https://en.wikipedia.org/wiki/IP_Code#Code_breakdown']

def redirect(name, redir):
    return [name, 'R', redir, '', '', '', '', '', '', '', '', '', '']

if __name__ == "__main__":
    output = []
    for (part_num, part_desc), (water_num, water_desc) in product(particles.items(), water.items()):
        name = "IP{}{}".format(part_num, water_num)
        desc = "Particle protection: {}<br>Water protection: {}".format(part_desc, water_desc)
        output.append(article(name, desc))
        output.append(redirect('IP {}{}'.format(part_num, water_num), name))

    for row in output:
        print '\t'.join(row)

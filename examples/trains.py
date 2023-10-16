import janus_swi as janus

janus.consult("trains","""
train('Amsterdam', 'Haarlem').
train('Amsterdam', 'Schiphol').
""")

print([d['Tuple'] for d in janus.query("train(_From,_To),Tuple=_From-_To")])

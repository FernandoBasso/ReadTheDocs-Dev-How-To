
#
# Blocks and procs are similar, but there is a key difference: you can only pass
# a single block to a method, but you can pass multiple procs (because procs
# are regular objects, unlike blocks).
#

def run_procs(prc1, prc2)
    prc1.call
    prc2.call
end

proc1 = Proc.new do
    p 'Proc 1'
end

proc2 = Proc.new do
    p 'Proc 2'
end

run_procs proc1, proc2

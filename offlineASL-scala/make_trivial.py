with open('./TrivialLifter.scala', 'r') as f:
    for l in f:
        if not 'override def' in l:
            print(l, end='')
            continue

        left = l.split(' = ', 1)[0]
        if '(' not in left:  # variable declaration
            print(l, end='')
            continue
        fname = left.split('(')[0].split()[-1]
        args = left.split('(', 1)[-1].split(')', 1)[0]
        argnames = [x.split(':')[0].strip() for x in args.split(',')]
        argnames = [x for x in argnames if x]
        nargs = len(argnames)

        if 'RTSym =' in l:  # emits a statement
            s = [f'"{fname}"'] + argnames
            s = ', '.join(s)
            print(l.split(' = ')[0] + f' = emit(Buffer({s}))')

        else:
            s = [f'"{fname}"'] + argnames
            s = ', '.join(s)
            print(l.split(' = ')[0] + f' = Buffer({s})')


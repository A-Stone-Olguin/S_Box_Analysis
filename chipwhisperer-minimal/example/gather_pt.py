import chipwhisperer as cw
import time


def gather_pt(N=100, hexname="flick_em.hex"):
    PLATFORM = "CWNANO"
    try:
        if not scope.connectStatus:
            scope.con()
    except NameError:
        scope = cw.scope()

    target_type = cw.targets.SimpleSerial

    try:
        cw.target(scope, target_type)
    except:
        print(
            "INFO: Caught exception on reconnecting to target - attempting to reconnect to scope first."
        )
        print(
            "INFO: This is a work-around when USB has died without Python knowing. Ignore errors above this line."
        )
        scope = cw.scope()
        cw.target(scope, target_type)

    if "STM" in PLATFORM or PLATFORM == "CWNANO":
        prog = cw.programmers.STM32FProgrammer
    else:
        prog = None

    print("INFO: Found ChipWhisperer😍")

    scope.default_setup()

    def reset_target(scope):
        scope.io.nrst = "low"
        time.sleep(0.05)
        scope.io.nrst = "high_z"
        time.sleep(0.05)

    cw.program_target(scope, prog, "../" + hexname)

    trace_array = []
    for _ in range(N):
        scope.arm()

        ret = scope.capture()
        if ret:
            print("Target timed out!")
            continue

        trace_array.append(scope.get_last_trace())

    # print(trace_array)

    scope.dis()
    return trace_array
gather_pt()

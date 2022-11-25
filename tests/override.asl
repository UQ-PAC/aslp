boolean HasArchVersion(ArchVersion version)
    return TRUE;

boolean InsertIESBBeforeException(bits(2) el)
    return TRUE;

SynchronizeErrors()
    return;

TakeUnmaskedPhysicalSErrorInterrupts(boolean iesb_req)
    return;

boolean HavePACExt()
    return FALSE;

SetTagCheckedInstruction(boolean checked)
    return;

AArch64.Abort(bits(64) vaddress, FaultRecord fault)
  assert FALSE;

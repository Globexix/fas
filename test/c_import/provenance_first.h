typedef int FasProvenanceFirstType;
int fas_provenance_first(int value);
#define FAS_PROVENANCE_DECLARE(name) int name(int value)
FAS_PROVENANCE_DECLARE(fas_provenance_macro);

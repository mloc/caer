#include "aco.h"

aco_t *aco_rshim_get_co() {
    return aco_gtls_co;
}

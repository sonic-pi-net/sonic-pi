#include <aubio.h>
#include "utils_tests.h"

void get_some_steps ( aubio_parameter_t * param );

void get_some_steps ( aubio_parameter_t * param )
{
  uint_t i = 0;
  uint_t steps = aubio_parameter_get_steps ( param );

  PRINT_MSG("next steps (%d) values:", steps );
  for (i = 0; i < steps; i ++ ) {
    PRINT_MSG(" %f", aubio_parameter_get_next_value (param) );
  }
  PRINT_MSG("\n");

  PRINT_MSG("next 3 values:");
  for (i = 0; i < 3; i ++ ) {
    PRINT_MSG(" %f", aubio_parameter_get_next_value (param) );
  }
  PRINT_MSG("\n");

}

int main (void)
{
  smpl_t max_value = 100.;
  smpl_t min_value = 0.;
  uint_t steps = 10;

  aubio_parameter_t * param = new_aubio_parameter ( min_value, max_value, steps );

  PRINT_MSG("initial value: %f, steps: %d\n", aubio_parameter_get_current_value
      (param) , aubio_parameter_get_steps (param) );

  PRINT_MSG("target: max_value / 2\n");
  aubio_parameter_set_target_value ( param, max_value );
  get_some_steps ( param );

  PRINT_MSG("target: max_value / 2\n");
  aubio_parameter_set_target_value ( param, max_value / 2 );
  get_some_steps ( param );

  PRINT_MSG("target: max_value * 2\n");
  aubio_parameter_set_target_value ( param, max_value * 2);
  get_some_steps ( param );

  PRINT_MSG("steps: 1, target: -max\n");
  aubio_parameter_set_steps ( param, 1);
  aubio_parameter_set_target_value ( param, - max_value);
  get_some_steps ( param );

  PRINT_MSG("steps: 30, current value: max, target: min\n");
  aubio_parameter_set_current_value ( param, max_value );
  aubio_parameter_set_target_value ( param, min_value );
  aubio_parameter_set_steps ( param, 7 );
  get_some_steps ( param );

  PRINT_MSG("steps: 30, max value: max * 2, min value: -max, current value: -max, target: max\n");
  aubio_parameter_set_min_value ( param, - max_value );
  aubio_parameter_set_max_value ( param, 2. * max_value );
  aubio_parameter_set_current_value ( param, - max_value );
  aubio_parameter_set_target_value ( param, max_value );
  aubio_parameter_set_steps ( param, 10 );
  get_some_steps ( param );

  del_aubio_parameter (param);

  return 0;
}

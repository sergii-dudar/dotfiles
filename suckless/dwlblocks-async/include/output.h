#ifndef OUTPUT_H
#define OUTPUT_H

typedef void output_connection;

output_connection* output_connection_open(void);
void output_connection_close(output_connection* const connection);
int output_set_status(output_connection* const connection,
                      const char* const name);

#endif  // OUTPUT_H

void blts(int);
void buts(int, double *);
void erhs();
void error();
void exact(int, int, int, double *);
void exchange_1(double *, int, int);
void exchange_3(double *, int);
void exchange_4(double *, double *, int, int, int, int);
void exchange_5(double *, int, int);
void exchange_6(double *, int, int);
void init_comm(int *, char ***);
void jacld(int);
void jacu(int);
void l2norm(int, int, int, double *, double *);
void neighbors();
void pintgr();
void print_results(char *, char *, int *,  int *, int *, int *,
                    int *, int *, double *, double *, char *,
                    int *, char *, char *, char *, char *, char *,
                    char *, char *, char *);
void proc_grid();
void bcast_inputs();
void read_input();
void rhs();
void setbv();
void setcoeff();
void setiv();
void ssor(int);
void subdomain();
void timer_clear(int *);
void timer_start(int *);
void timer_stop(int *);
void verify(double *, double *, double *, char *);
int  nodedim();
double timer_read(int *);
double test_rsd();


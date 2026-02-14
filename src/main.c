extern void wait (void);
extern void __fastcall__ monitor_init (void);

int main (void) {
    monitor_init();
    while (1) {
        wait();
    }
    return 0;
}

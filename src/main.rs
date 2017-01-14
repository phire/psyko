mod cpu;
use cpu::CpuDomain;

fn main() {
    let mut cpu_domain = CpuDomain::new();
    cpu_domain.run(20);
}
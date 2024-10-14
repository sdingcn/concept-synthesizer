template <typename T>
void f(T x) {
    x.F();
}

template <typename U>
void g(U x) {
    x.G();
}

template <typename V>
void h(V x) {
    x++;
    if (x) {
        f(x);
    } else {
        g(x);
    }
}

int main() {
}

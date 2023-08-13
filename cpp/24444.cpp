#include <iostream>
#include <vector>
#include <deque>
#include <algorithm>

using namespace std;

inline constexpr int MAX_NODE = 100000;

int n, m, r, u, v, order = 0;
int visited[MAX_NODE + 1];
int res[MAX_NODE + 1];
vector<int> graph[MAX_NODE + 1];

void bfs(int node) {
    deque<int> dq;

    visited[node] = 1;
    order += 1;
    res[node] = order;
    dq.push_back(node);

    while (!dq.empty()) {
        int front = dq[0];
        dq.pop_front();
        for (int adj : graph[front]) {
            if (!visited[adj]) {
                visited[adj] = 1;
                dq.push_back(adj);
                order += 1;
                res[adj] = order;
            }
        }
    }
}

int main() {
    cin >> n >> m >> r;

    for (int i = 0; i < m; i++) {
        cin >> u >> v;
        graph[u].push_back(v);
        graph[v].push_back(u);
    }

    for (int i = 1; i <= n; i++) {
        sort(graph[i].begin(), graph[i].end());
    }

    bfs(r);

    for (int i = 1; i <= n; i++) {
        cout << res[i] << '\n';
    }
}
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

inline constexpr int MAX_NODE = 100000;

int n, m, r, u, v, order = 0;
int visited[MAX_NODE + 1];
int res[MAX_NODE + 1];
vector<int> graph[MAX_NODE + 1];

void dfs(int node) {
    visited[node] = 1;
    order += 1;
    res[node] = order;

    for (int adj : graph[node]) {
        if (!visited[adj])
            dfs(adj);
    }
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    cin >> n >> m >> r;

    for (int i = 0; i < m; i++) {
        cin >> u >> v;
        graph[u].push_back(v);
        graph[v].push_back(u);
    }

    for (int i = 1; i <= n; i++) {
        sort(graph[i].rbegin(), graph[i].rend());
    }

    dfs(r);

    for (int i = 1; i <= n; i++) {
        cout << res[i] << '\n';
    }
}
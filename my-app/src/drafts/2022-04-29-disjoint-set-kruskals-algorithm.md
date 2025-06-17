# Disjoint set, union-find & Kruskal's algorithm

Leetcode problem [#1584, "Min Cost to Connect All Points"](https://leetcode.com/problems/min-cost-to-connect-all-points/).

```
A0=(-8,14)
A1=(16,-18)
A2=(-19,-13)
A3=(-18,19)
A4=(20,20)
A5=(13,-20)
A6=(-15,9)
A7=(-4,-8)

e0=Segment(A1,A5)
e1=Segment(A0,A6)
e2=Segment(A3,A6)
e3=Segment(A2,A7)
e4=Segment(A2,A6)
e5=Segment(A5,A7)
-- e6=Segment(A1,A7)
e7=Segment(A0,A4)
```

See this visualization [on Geogebra](https://www.geogebra.org/calculator/nsezb8y7).

Other Disjoint Set implementations:

* https://www.hackerearth.com/practice/notes/disjoint-set-union-union-find/
* https://www.techiedelight.com/disjoint-set-data-structure-union-find-algorithm/
* https://cp-algorithms.com/data_structures/disjoint_set_union.html

```java
class Solution {
    private static class Edge {
        public int from;
        public int to;
        public int weight;

        public Edge(int a, int b, int weight) {
            this.from = a;
            this.to = b;
            this.weight = weight;
        }

        @Override
        public int hashCode() {
            return Objects.hash(from, to);
        }

        @Override
        public boolean equals(Object other) {
            if (other == null || other.getClass() != this.getClass()) {
                return false;
            }

            Edge o = (Edge) other;
            return (o.from == from && o.to == to) || (o.to == from && o.from == to);
        }
    }

    private static class DisjointSet {
        private int[] components;
        private int[] ranks;

        public DisjointSet(int n) {
            components = new int[n];
            ranks = new int[n];

            for (int i = 0; i < n; ++i) {
                components[i] = i;
                ranks[i] = 1;
            }
        }

        public void union(int eltA, int eltB) {
            int parentA = find(eltA);
            int parentB = find(eltB);

            if (parentA == parentB) {
                return;
            }

            // tricky, but important: assign the values of __parents__, thus joining __parents__ not elements themselves
            if (ranks[parentB] > ranks[parentA]) {
                components[parentA] = parentB;
                ranks[parentB] += ranks[parentA];
            } else {
                components[parentB] = parentA;
                ranks[parentA] += ranks[parentB];
            }
        }

        public int find(int elt) {
            if (elt == components[elt]) {
                return elt;
            }

            return find(components[elt]);
        }

        public void debug() {
            System.out.printf("components: [ ");

            for (int i = 0; i < components.length; ++i) {
                System.out.printf("%d/%d ", components[i], ranks[i]);
            }

            System.out.printf("]\n");
        }
    }

    public int minCostConnectPoints(int[][] points) {
        int n = points.length;

        DisjointSet set = new DisjointSet(n);

        PriorityQueue<Edge> queue = new PriorityQueue<>(Comparator.comparingInt(e -> e.weight));

        for (int i = 0; i < n; ++i) {
            for (int t = i + 1; t < n; ++t) {
                int d = Math.abs(points[i][0] - points[t][0]) + Math.abs(points[i][1] - points[t][1]);

                // System.out.printf("> e(%d, %d) = %d\n", i, t, d);

                queue.add(new Edge(i, t, d));
            }
        }

        int cost = 0;
        int edgesSelected = 0;

        // set.debug();

        while (!queue.isEmpty() && edgesSelected < n - 1) {
            Edge e = queue.remove();

            int parentFrom = set.find(e.from);
            int parentTo = set.find(e.to);

            if (parentFrom == parentTo) {
                continue;
            }

            cost += e.weight;
            edgesSelected++;

            set.union(e.from, e.to);

            // System.out.printf("== e(%d, %d) = %d; (parent(%d), parent(%d))\n", e.from, e.to, e.weight, parentFrom, parentTo);
            // set.debug();
        }

        return cost;
    }
}
```

```
components: [ 0/1 1/1 2/1 3/1 4/1 5/1 6/1 7/1 ]
== e(1, 5) = 5; (parent(1), parent(5))
components: [ 0/1 1/2 2/1 3/1 4/1 1/1 6/1 7/1 ]
== e(0, 6) = 12; (parent(0), parent(6))
components: [ 0/2 1/2 2/1 3/1 4/1 1/1 0/1 7/1 ]
== e(3, 6) = 13; (parent(3), parent(0))
components: [ 0/3 1/2 2/1 0/1 4/1 1/1 0/1 7/1 ]
== e(2, 7) = 20; (parent(2), parent(7))
components: [ 0/3 1/2 2/2 0/1 4/1 1/1 0/1 2/1 ]
== e(2, 6) = 26; (parent(2), parent(0))
components: [ 0/5 1/2 0/2 0/1 4/1 1/1 0/1 0/1 ]
== e(5, 7) = 29; (parent(1), parent(0))
components: [ 0/7 0/2 0/2 0/1 4/1 0/1 0/1 0/1 ]
== e(0, 4) = 34; (parent(0), parent(4))
components: [ 0/8 0/2 0/2 0/1 0/1 0/1 0/1 0/1 ]
```

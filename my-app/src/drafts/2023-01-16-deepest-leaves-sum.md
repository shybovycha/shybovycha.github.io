## Leet "deepest leaves sum" (#1302)

> Given the root of a binary tree, return the sum of values of its deepest leaves.

Input: root = [1,2,3,4,5,null,6,7,null,null,null,null,8]
Output: 15

Input: root = [6,7,8,2,7,1,3,9,null,1,4,null,null,null,5]
Output: 19

Constraints:

* The number of nodes in the tree is in the range [1, 104].
* 1 <= Node.val <= 100

## BFS

```java
/**
 * Definition for a binary tree node.
 * public class TreeNode {
 *     int val;
 *     TreeNode left;
 *     TreeNode right;
 *     TreeNode() {}
 *     TreeNode(int val) { this.val = val; }
 *     TreeNode(int val, TreeNode left, TreeNode right) {
 *         this.val = val;
 *         this.left = left;
 *         this.right = right;
 *     }
 * }
 */
class Solution {
    private static class Node {
        public TreeNode node;
        public int level;

        public Node(TreeNode node, int level) {
            this.node = node;
            this.level = level;
        }
    }

    public int deepestLeavesSum(TreeNode root) {
        Deque<Node> queue = new ArrayDeque<>();

        int maxLevel = 0;

        queue.addLast(new Node(root, 0));

        while (!queue.isEmpty()) {
            Node node = queue.removeLast();

            if (node.level > maxLevel) {
                maxLevel = node.level;
            }

            if (node.node.left != null) {
                queue.addLast(new Node(node.node.left, node.level + 1));
            }

            if (node.node.right != null) {
                queue.addLast(new Node(node.node.right, node.level + 1));
            }
        }

        int sum = 0;

        queue.addLast(new Node(root, 0));

        while (!queue.isEmpty()) {
            Node node = queue.removeLast();

            if (node.level == maxLevel) {
                sum += node.node.val;
            }

            if (node.node.left != null) {
                queue.addLast(new Node(node.node.left, node.level + 1));
            }

            if (node.node.right != null) {
                queue.addLast(new Node(node.node.right, node.level + 1));
            }
        }

        return sum;
    }
}
```

## BFS2

```java
class Solution {
    private static class Node {
        public TreeNode node;
        public int level;

        public Node(TreeNode node, int level) {
            this.node = node;
            this.level = level;
        }
    }

    public int deepestLeavesSum(TreeNode root) {
        Deque<Node> queue = new ArrayDeque<>();

        int maxLevel = 0;
        int sum = 0;

        queue.addLast(new Node(root, 0));

        while (!queue.isEmpty()) {
            Node node = queue.removeLast();

            if (node.level > maxLevel) {
                maxLevel = node.level;
                sum = node.node.val;
            } else if (node.level == maxLevel) {
                sum += node.node.val;
            }

            if (node.node.left != null) {
                queue.addLast(new Node(node.node.left, node.level + 1));
            }

            if (node.node.right != null) {
                queue.addLast(new Node(node.node.right, node.level + 1));
            }
        }

        return sum;
    }
}
```

## DFS

```java
class Solution {
    private int findMaxLevel(TreeNode root, int level, int maxLevel, Map<Integer, Integer> cache) {
        if (root == null) {
            return level - 1;
        }

        if (!cache.containsKey(level)) {
            cache.put(level, root.val);
        } else {
            cache.put(level, cache.get(level) + root.val);
        }

        return Math.max(findMaxLevel(root.left, level + 1, maxLevel, cache), findMaxLevel(root.right, level + 1, maxLevel, cache));
    }

    public int deepestLeavesSum(TreeNode root) {
        Map<Integer, Integer> cache = new HashMap<>();

        int maxLevel = findMaxLevel(root, 0, 0, cache);

        return cache.get(maxLevel);
    }
}
```

## WTF

```java
class Solution {
    private static class Pair {
        public int level;
        public int sum;

        public Pair() {
            level = 0;
            sum = 0;
        }
    }

    private void findSum(TreeNode root, int level, Pair maxLevel) {
        if (root == null) {
            return;
        }

        if (level > maxLevel.level) {
            // System.out.printf("new max level %d with sum %d\n", level, root.val);
            maxLevel.level = level;
            maxLevel.sum = root.val;
        } else if (level == maxLevel.level) {
            // System.out.printf("same max level %d with new sum %d\n", level, maxLevel.sum + root.val);
            maxLevel.sum += root.val;
        }

        findSum(root.left, level + 1, maxLevel);
        findSum(root.right, level + 1, maxLevel);
    }

    public int deepestLeavesSum(TreeNode root) {
        Pair maxLevel = new Pair();

        findSum(root, 0, maxLevel);

        return maxLevel.sum;
    }
}
```
## Longest increasing subsequence

Leetcode problem [#300, Longest increasing subsequence](https://leetcode.com/problems/longest-increasing-subsequence/)

## Dynamic programming approach

### Top-to-bottom

This is a more natural solution, easier to understand once you dive deep into dynamic programming.
However, it runs _slightly_ slower than the bottom-up solution (98ms vs 78ms).

```java
class Solution {
    private int lengthOfLISRec(int[] nums, int start, int[] cache) {
        if (cache[start] > -1) {
            return cache[start];
        }

        int len = 1;

        for (int i = start; i < nums.length; ++i) {
            if (nums[i] > nums[start]) {
                len = Math.max(len, 1 + lengthOfLISRec(nums, i, cache));
            }
        }

        cache[start] = len;

        return len;
    }

    public int lengthOfLIS(int[] nums) {
        int[] cache = new int[nums.length];

        Arrays.fill(cache, -1);

        int len = 1;

        for (int i = 0; i < nums.length; ++i) {
            len = Math.max(len, lengthOfLISRec(nums, i, cache));
        }

        return len;
    }
}
```

### Bottom-up

Here we build the solutions for the subproblems by solving the simplest case first and then use them to solve the larger problem.

```java
class Solution {
    public int lengthOfLIS(int[] nums) {
        int[] cache = new int[nums.length];
        int maxLength = 1;

        cache[nums.length - 1] = 1;

        for (int i = nums.length - 2; i > -1; --i) {
            int len = 1;

            for (int t = i + 1; t < nums.length; ++t) {
                if (nums[i] < nums[t] && len < 1 + cache[t]) {
                    len = 1 + cache[t];
                }
            }

            cache[i] = len;

            if (cache[i] > maxLength) {
                maxLength = cache[i];
            }
        }

        return maxLength;
    }
}
```


## Patience sorting

This is an algorithm best explained with a standard deck of playing cards.

A quite poor (in my opinion) explanation is given on GeeksForGeeks website: https://www.geeksforgeeks.org/longest-monotonically-increasing-subsequence-size-n-log-n/
The demonstration is available on Princeton's university resources: https://www.cs.princeton.edu/courses/archive/spring13/cos423/lectures/LongestIncreasingSubsequence.pdf

```java
class Solution {
    public int lengthOfLIS(int[] nums) {
        int[] cache = new int[nums.length];

        Arrays.fill(cache, 1);

        int maxLength = 1;

        for (int i = nums.length - 1; i > -1; --i) {
            for (int t = i + 1; t < nums.length; ++t) {
                if (nums[t] > nums[i]) {
                    cache[t]++;

                    maxLength = Math.max(maxLength, cache[t]);
                }
            }
        }

        for (int i = 0; i < nums.length; ++i) {
            System.out.printf("%d ", cache[i]);
        }

        return maxLength;
    }
}
```

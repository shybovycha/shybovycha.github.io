This is a review of some ways to solve the [K closest points to origin](https://leetcode.com/problems/k-closest-points-to-origin/description/) problem on LeetCode.

Problem statement:

> Given an array of points where `points[i] = [xi, yi]` represents a point on the X-Y plane and an integer `k`, return the `k` closest points to the origin `(0, 0)`.
>
> The distance between two points on the X-Y plane is the Euclidean distance (i.e., `sqrt((x1 - x2)^2 + (y1 - y2)^2)`).
>
> You may return the answer in any order. The answer is guaranteed to be unique (except for the order that it is in).
>
> Constraints:
> * `1 <= k <= points.length <= 10^4`
> * `-10^4 <= xi, yi <= 10^4`

The first idea I had was to use the priority queue:

```cpp
class Solution {
public:
    vector<vector<int>> kClosest(vector<vector<int>>& points, int k) {
        auto comparator = [](const std::vector<int>& p1, const std::vector<int>& p2) {
            return std::sqrt((p1[0] * p1[0]) + (p1[1] * p1[1])) > std::sqrt((p2[0] * p2[0]) + (p2[1] * p2[1]));
        };

        std::priority_queue<std::vector<int>, std::vector<std::vector<int>>, decltype(comparator)> q { comparator };

        for (auto& point : points) {
            q.push(point);
        }

        std::vector<std::vector<int>> result(k);

        for (auto i = 0; i < k; ++i) {
            result[i] = q.top();
            q.pop();
        }

        return result;
    }
};
```

It was a successful submission, which beated 47.93% of other submissions in time and 38.73% in memory.

Then I thought: the comparator would be called roughly `O(n * log(n))` times. Which is suboptimal.
So why not pre-compute all the distances for every point and store them in memory and then just sort on that attribute?
This would call comparator only `O(n)` times:

```cpp
struct Point {
    int x;
    int y;
    float d;
};

class Solution {
public:
    vector<vector<int>> kClosest(vector<vector<int>>& points, int k) {
        auto points2 = std::vector<Point>(points.size());

        std::transform(points.begin(), points.end(), points2.begin(), [](const std::vector<int>& p) {
            return Point { .x = p[0], .y = p[1], .d = static_cast<float>(std::sqrt((p[0] * p[0]) + (p[1] * p[1]))) };
        });

        std::sort(points2.begin(), points2.end(), [](const Point& a, const Point& b) {
            return a.d < b.d;
        });

        auto result = std::vector<std::vector<int>>(k);

        for (auto i = 0; i < k; ++i) {
            result[i] = std::vector<int> { points2[i].x, points2[i].y };
        }

        return result;
    }
};
```

This solution beated 79.83% others in time and 87.74% in memory.

Next, considering the requirements of having at most `10^4` points is not a lot (`2 * sizeof(int) * 10^4 = 80kB`) so I thought
storing the arrays on stack instead of a heap would be _much_ faster. But C++ is not good for that, so I had to go with C:

```c
/**
 * Return an array of arrays of size *returnSize.
 * The sizes of the arrays are returned as *returnColumnSizes array.
 * Note: Both returned array and *columnSizes array must be malloced, assume caller calls free().
 */
struct Point {
    int x;
    int y;
    float d;
};

int** kClosest(int** points, int pointsSize, int* pointsColSize, int k, int* returnSize, int** returnColumnSizes){
    struct Point ps[pointsSize];
    struct Point tmp;

    for (int i = 0; i < pointsSize; ++i) {
        tmp.x = points[i][0];
        tmp.y = points[i][1];
        tmp.d = (float) sqrt((tmp.x * tmp.x) + (tmp.y * tmp.y));
        ps[i] = tmp;
    }

    // ...

    int** resPointers = (int**) malloc(k * sizeof(int*));
    int* resSizes = (int*) malloc(k * sizeof(int));
    int* res = (int*) malloc(k * 2 * sizeof(int));

    for (int i = 0; i < k; ++i) {
        res[i * 2] = ps[i].x;
        res[(i * 2) + 1] = ps[i].y;
        resPointers[i] = &res[i * 2];
        resSizes[i] = 2;
    }

    *returnSize = k;
    *returnColumnSizes = resSizes;

    return resPointers;
}
```

The remaining question was: how to sort the _stack-allocated_ array?

The simple insertion sort timed out. And that is understandable - it has `O(n^2)` time complexity. So it is slow even on stack.

The next idea I had was to sort elements as we insert them into an array, but that failed terribly

```c
int** kClosest(int** points, int pointsSize, int* pointsColSize, int k, int* returnSize, int** returnColumnSizes){
    struct Point ps[pointsSize];

    float maxDistanceInKWindow = 0.0f;
    float minDistanceInKWindow = 14143.0f; // max distance possible - sqrt(2 * 10^8)
    int lastIndexInKWindow = 0;
    int lastIndexOutKWindow = 0;

    struct Point tmp;

    for (int i = 0; i < pointsSize; ++i) {
        struct Point p;
        p.x = points[i][0];
        p.y = points[i][1];
        p.d = (float) sqrt((p.x * p.x) + (p.y * p.y));

        if (lastIndexInKWindow < k) {
            printf("[%d<k];", lastIndexInKWindow);

            ps[lastIndexInKWindow++] = p;

            if (p.d < minDistanceInKWindow) {
                printf("%d,%d (%d) is less than min\n", p.x, p.y, lastIndexInKWindow);
                minDistanceInKWindow = p.d;

                if (lastIndexInKWindow > 1) {
                    printf("[swap at %d, %d]", 0, lastIndexInKWindow);
                    tmp = ps[0];
                    ps[0] = p;
                    ps[lastIndexInKWindow] = tmp;
                }
            }

            if (p.d > maxDistanceInKWindow) {
                printf("%d,%d (%d) is more than max\n", p.x, p.y, lastIndexInKWindow);
                maxDistanceInKWindow = p.d;
            }
        } else {
            printf("[%d>=k];", k + lastIndexOutKWindow);

            if (p.d < minDistanceInKWindow) {
                printf("%d,%d (%d ~ %d) is within the full window and is less than min\n", p.x, p.y, 0, k + lastIndexOutKWindow);
                printf("[swap at %d, %d]", 0, k + lastIndexOutKWindow + 1);

                minDistanceInKWindow = p.d;
                tmp = ps[0];
                ps[0] = p;
                ps[k + lastIndexOutKWindow++] = tmp;
            } else if (p.d < maxDistanceInKWindow) {
                printf("%d,%d (%d ~ %d) is within the full window and is less than max\n", p.x, p.y, k - 1, k + lastIndexOutKWindow);
                printf("[swap at %d, %d]", k - 1, k + lastIndexOutKWindow + 1);

                maxDistanceInKWindow = p.d;
                tmp = ps[k - 1];
                ps[k - 1] = p;
                ps[k + lastIndexOutKWindow++] = tmp;
            } else {
                printf("%d,%d (%d) is whatever\n", p.x, p.y, k + lastIndexOutKWindow);
                printf("[put at %d]", k + lastIndexOutKWindow + 1);

                ps[k + lastIndexOutKWindow++] = p;
            }
        }
    }

    int** resPointers = (int**) malloc(k * sizeof(int*));
    int* resSizes = (int*) malloc(k * sizeof(int));
    int* res = (int*) malloc(k * 2 * sizeof(int));

    for (int i = 0; i < k; ++i) {
        res[i * 2] = ps[i].x;
        res[(i * 2) + 1] = ps[i].y;
        resPointers[i] = &res[i * 2];
        resSizes[i] = 2;
    }

    *returnSize = k;
    *returnColumnSizes = resSizes;

    return resPointers;
```

This algorithm failed, as it was replacing the elements in the thought-to-be-sorted window of `k` elements which it should have not.

```c
/**
 * Return an array of arrays of size *returnSize.
 * The sizes of the arrays are returned as *returnColumnSizes array.
 * Note: Both returned array and *columnSizes array must be malloced, assume caller calls free().
 */
struct Point {
    int x;
    int y;
    float d;
};

int** kClosest(int** points, int pointsSize, int* pointsColSize, int k, int* returnSize, int** returnColumnSizes){
    struct Point ps[pointsSize];
    struct Point tmp;

    int largest = 0;
    int left = 0;
    int right = 0;

    for (int i = 0; i < pointsSize; ++i) {
        tmp.x = points[i][0];
        tmp.y = points[i][1];
        tmp.d = (float) sqrt((tmp.x * tmp.x) + (tmp.y * tmp.y));
        ps[i] = tmp;
    }

    // printf("heapifyng (1)\n");

    // heapify
    for (int i = pointsSize / 2 - 1; i >= 0; --i) {
        while (1) {
            largest = i;

            left = 2 * i + 1;
            right = 2 * i + 2;

            if (left < pointsSize && ps[left].d < ps[largest].d)
                largest = left;

            if (right < pointsSize && ps[right].d < ps[largest].d)
                largest = right;

            if (largest == i)
                break;

            // if (largest != i) {
                tmp = ps[i];
                ps[i] = ps[largest];
                ps[largest] = tmp;

                // heapify(arr, pointsSize, largest);
                i = largest;
            // }
        }
    }

    // printf("mallocs (2)\n");

    int** resPointers = (int**) malloc(k * sizeof(int*));
    int* resSizes = (int*) malloc(k * sizeof(int));
    int* res = (int*) malloc(k * 2 * sizeof(int));

    // printf("de-heapifyng (3)\n");

    int t = 0;
    int i = 0;

    // pop
    for (int j = 0; j < k; ++j) {
        i = pointsSize - 1 - j;

        // printf("pop %d\n", i);

        tmp = ps[0];
        ps[0] = ps[i];
        ps[i] = tmp;

        // printf("> mem assignments\n");

        res[j * 2] = ps[i].x; // printf(">> res[%d * 2]\n", j * 2);
        res[(j * 2) + 1] = ps[i].y; // printf(">> res[(%d * 2) + 1]\n", j);
        resPointers[j] = &res[j * 2]; // printf(">> resPointers[%d]\n", j);
        resSizes[j] = 2; // printf(">> resSizes[%d]\n", j);

        // printf("> re-heapify\n");

        t = 0;

        // heapify again, arg_N = i, arg_i = 0
        while (1) {
            largest = t;

            left = 2 * t + 1;
            right = 2 * t + 2;

            if (left < i && ps[left].d < ps[largest].d)
                largest = left;

            if (right < i && ps[right].d < ps[largest].d)
                largest = right;

            if (largest == t)
                break;

            // if (largest != i) {
                tmp = ps[t];
                ps[t] = ps[largest];
                ps[largest] = tmp;

                // heapify(arr, pointsSize, largest);
                t = largest;
            // }
        }
    }

    // for (int i = 0; i < k; ++i) {
    //     res[i * 2] = ps[i].x;
    //     res[(i * 2) + 1] = ps[i].y;
    //     resPointers[i] = &res[i * 2];
    //     resSizes[i] = 2;
    // }

    *returnSize = k;
    *returnColumnSizes = resSizes;

    return resPointers;

    /*
    // memory layout: k*(tuples of (int x, int y)) ;; k*(pointer to i-th element of previous section) ;; k*(sizes of tuples, each eq 2)
    int* res = (int*) malloc((k * 2 * sizeof(int)) + (k * sizeof(int*)) + (k * sizeof(int)));

    int off1 = k * 2 * sizeof(int);
    int off2 = off1 + (k * sizeof(int*));

    int** resPointers = (int**) (res + off1);
    int* resSizes = res + off2;

    for (int i = 0; i < k; ++i) {
        res[i * 2] = ps[i].x;
        res[(i * 2) + 1] = ps[i].y;

        resSizes[i] = 2;
        resPointers[i] = res + (i * 2);
    }

    *returnSize = k;
    *returnColumnSizes = resSizes;

    return resPointers;
    */
}
```

I also thought about making just one `malloc` call instead of 3 and operating a newly allocated block of memory for 3 different purposes:

```c
// memory layout: k*(tuples of (int x, int y)) ;; k*(pointer to i-th element of previous section) ;; k*(sizes of tuples, each eq 2)
int* res = (int*) malloc((k * 2 * sizeof(int)) + (k * sizeof(int*)) + (k * sizeof(int)));

int off1 = k * 2 * sizeof(int);
int off2 = off1 + (k * sizeof(int*));

int** resPointers = (int**) (res + off1);
int* resSizes = res + off2;

for (int i = 0; i < k; ++i) {
    res[i * 2] = ps[i].x;
    res[(i * 2) + 1] = ps[i].y;

    resSizes[i] = 2;
    resPointers[i] = res + (i * 2);
}

*returnSize = k;
*returnColumnSizes = resSizes;

return resPointers;
```

I even tried C++23 ranges:

```cpp
vector<vector<int>> kClosest(vector<vector<int>>& points, int k) {
    std::array<std::array<int, 2>, 10000> arr{};

    int i = 0;

    for (auto& p : points) {
        arr[i++] = std::array<int, 2>{ p[0], p[1] };
    }

    std::ranges::sort(arr.begin(), arr.end(), [](auto& p) { return (p[0] * p[0]) + (p[1] * p[1]); });

    return arr | std::views::take(k);
}
```

This solution beats `99.75%` others in terms of memory, but only `5.07%` in terms of time.

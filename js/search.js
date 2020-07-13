const getSearchIndexData = () => {
  const cachedSearchIndex = window.localStorage.getItem('search_index');

  if (cachedSearchIndex) {
    return Promise.resolve(JSON.parse(cachedSearchIndex));
  }

  return fetch('/word_list.json');
};

const cacheSearchIndexData = (index) => {
  window.localStorage.setItem('search_index', JSON.stringify(index));

  return index;
};

const buildIndex = ({ words, documents }) => {
  const index = new TrieMap();

  Object.entries(words)
    .forEach(([ word, occurrences ]) => index.add(word, occurrences));

  return index;
};

const search = (query, index) => {
  const queryWords = query.split(/\W+/);

  const tmpResults = queryWords.map(queryWord => index.find(queryWord))
    .reduce((acc, [ docId, occurrences ]) => { acc[docId] = (acc[docId] || 0) + 100; return acc; }, {});

  const results = new MaxHeap();

  Object.entries(tmpResults).forEach(([ docId, score ]) => results.add(docId, score));

  const sortedResults = [];

  while (results.length > 0) {
    sortedResults.push(results.remove().value);
  }

  return sortedResults;
};

const debounce = (func, timeout) => {
  let timeoutId;

  return (...args) => {
    clearTimeout(timeoutId);

    timeoutId = setTimeout(() => {
      func.apply(null, args);
    }, timeout);

    if (!timeoutId) func.apply(null, args);
  };
};

const addSearchHandler = (index) => {
  document.querySelector('#search').addEventListener('keydown', 300, debounce((event) => {
      const query = event.target.text;

      const results = search(query, index);

      console.log('Searching for:', query);
      console.log('Search results:', results);
    }));
};

document.addEventListener('DOMContentReady', () => {
  getSearchIndexData()
    .then(cacheSearchIndexData)
    .then(buildIndex)
    .then(addSearchHandler);
});


// -------


class Node {
  constructor(key, value = null, children = {}) {
    this.key = key;
    this.value = value;
    this.children = children;
    this.endsWord = false;
  }

  serialize() {
    return [this.key, this.value, this.endsWord ? 1 : 0, Object.values(this.children).map(c => c.serialize())];
  }

  static deserialize([ key, value, endsWord, children ]) {
    const node = new Node();

    node.key = key;
    node.value = value;
    node.endsWord = new Boolean(endsWord);

    node.children = children
      .map(c => Node.deserialize(c))
      .reduce((acc, node) => Object.assign(acc, { [node.key]: node }), {});

    return node;
  }
}

class TrieMap {
  constructor() {
    this.root = new Node(null);
  }

  put(key, value) {
    let node = this.root;

    for (let i = 0; i < key.length; ++i) {
      const ch = key.charAt(i);

      let child = node.children[ch];

      if (!child) {
        child = new Node(ch);
        node.children[ch] = child;
      }

      node = child;
    }

    node.endsWord = true;
    node.value = value;
  }

  get(key) {
    let node = this.root;

    for (let i = 0; i < key.length; ++i) {
      const ch = key.charAt(i);

      let child = node.children[ch];

      if (!child) {
        return undefined;
      }

      node = child;
    }

    if (node.endsWord) {
      return node.value;
    }

    return undefined;
  }

  keys() {
    let queue = [[this.root, []]];
    let results = [];

    while (queue.length > 0) {
      const [ node, prevPath ] = queue.shift();

      const path = (node === this.root) ? prevPath : prevPath.concat([ node.key ]);

      if (node.endsWord) {
        results.push(path.join(''));
      }

      for (let child of Object.values(node.children)) {
        queue.push([ child, path ]);
      }
    }

    return results;
  }

  toJSON() {
    return JSON.stringify(this.root.serialize());
  }

  static fromJSON(json) {
    const map = new TrieMap();

    map.root = Node.deserialize(JSON.parse(json));

    return map;
  }

  findByPrefix(key) {
    let node = this.root;

    for (let i = 0; i < key.length; ++i) {
      const ch = key.charAt(i);

      let child = node.children[ch];

      if (!child) {
        return [];
      }

      node = child;
    }

    let children = Object.values(node.children);
    let results = [];

    if (node.endsWord) {
      results.push(node.value);
    }

    while (children.length > 0) {
      node = children.shift();

      if (node.endsWord) {
        results.push(node.value);
      }

      children = children.concat(Object.values(node.children));
    }

    return results;
  }
}

// var m = new TrieMap();

// // full format:
// // Object.entries(word_list.words).forEach(([ word, { occurrences } ]) => { m.put(word, Object.entries(occurrences).map(([ docId, { positions } ]) => ({ [docId]: positions }))) })

// // compact format:
// Object.entries(word_list.words).forEach(([ word, occurrences ]) => m.put(word, occurrences));

// // searching in TrieMap:
// const tfidf = occurrences => Object.values(occurrences).reduce((acc, e) => acc + e.length, 0) / Object.keys(occurrences).length;

// const results = query.split(/\W+/).flatMap(term => m.findByPrefix(term) )
//     .sort((a, b) => tfidf(b) - tfidf(a))
//     .flatMap(e => Object.keys(e).map(docId => word_list.documents[docId]))

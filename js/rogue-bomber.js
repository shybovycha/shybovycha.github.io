const __startRogueBomber = (canvas) => {
  const ctx = canvas.getContext('2d');

  const TILE_HEIGHT = 24;
  ctx.font = `${TILE_HEIGHT}px courier new`;
  const TILE_WIDTH = ctx.measureText('X').width;

  const UI_WIDTH = TILE_WIDTH * 30;

  const TILE_EMPTY = Symbol(0);
  const TILE_PERMANENT_WALL = Symbol(1);
  const TILE_BRICK_WALL = Symbol(2);
  const TILE_PLAYER = Symbol(3);
  const TILE_BOMB1 = Symbol(4);
  const TILE_BOMB2 = Symbol(5);
  const TILE_EXPLOSION = Symbol(6);
  const TILE_NEXT_LEVEL_DOOR0 = Symbol(7);
  const TILE_NEXT_LEVEL_DOOR1 = Symbol(8);
  const TILE_NEXT_LEVEL_DOOR2 = Symbol(9);

  const BONUS_BOMB1 = Symbol('ò+1');
  const BONUS_BOMB2 = Symbol('ò+2');
  const BONUS_FIRE1 = Symbol('!+1');
  const BONUS_FIRE2 = Symbol('!+2');
  const BONUS_LIFE1 = Symbol('++1');
  const BONUS_LIFE2 = Symbol('++2');

  const BONUS_TYPE = [ BONUS_BOMB1, BONUS_FIRE1, BONUS_LIFE1 ];

  const ENEMY_LAMP = Symbol('l');
  const ENEMY_BOAR = Symbol('b');
  const ENEMY_BROOM = Symbol('B');

  const INVINCIBLE_PLAYER = Symbol('i@i');

  const ENEMY_TYPE = [ ENEMY_LAMP ]; //, ENEMY_BOAR, ENEMY_BROOM ];

  const TILES = {
    [TILE_EMPTY]: ['.', '#b3b3b3'], // empty
    [TILE_PERMANENT_WALL]: ['#', '#a2a2a2'], // permanent wall
    [TILE_BRICK_WALL]: ['▒', '#b76014'], // '░' brick wall
    [TILE_PLAYER]: ['@', '#000'], // player
    [TILE_BOMB1]: ['ó', '#000'], // bomb
    [TILE_BOMB2]: ['Ó', '#000'], // bomb
    [TILE_EXPLOSION]: ['*', '#ff8920'], // explosion
    [TILE_NEXT_LEVEL_DOOR0]: ['>', '#bebebe'], // next level door
    [TILE_NEXT_LEVEL_DOOR1]: ['>', '#0bacac'], // next level door
    [TILE_NEXT_LEVEL_DOOR2]: ['>', '#2edfdf'], // next level door
    [ENEMY_LAMP]: ['l', '#fbc546'], // enemy: lamp
    [ENEMY_BOAR]: ['b', '#b05d13'], // enemy: boar
    [ENEMY_BROOM]: ['B', '#e8c129'], // enemy: broom
    [INVINCIBLE_PLAYER]: ['@', '#c7eeea'], // invincible player blink
    [BONUS_BOMB1]: ['ò', '#0bacac'], // bonus bomb
    [BONUS_BOMB2]: ['ò', '#2edfdf'], // bonus bomb blink
    [BONUS_FIRE1]: ['!', '#fbb40c'], // bonus fire
    [BONUS_FIRE2]: ['!', '#f7c142'], // bonus fire blink
    [BONUS_LIFE1]: ['+', '#307907'], // bonus life
    [BONUS_LIFE2]: ['+', '#52d508'], // bonus life blink
  };

  const KEY_MOVE_RIGHT = 72; // h
  const KEY_MOVE_LEFT = 74; // j
  const KEY_MOVE_UP = 75; // k
  const KEY_MOVE_DOWN = 76; // l
  const KEY_PLACE_BOMB = 66; // b

  const BOMB_TICK_INTERVAL = 700;
  const BOMB_TICK_TIME = 4100;
  const EXPLOSION_DURATION = 600;

  const INVINCIBLE_TIMEOUT = 2000;
  const INVINCIBLE_INTERVAL = 200;

  const NEXT_LEVEL_DOOR_INTERVAL = 150;

  const BONUS_UPDATE_INTERVAL = 200;
  const BONUS_START_BLINK_TIME = 3000;
  const BONUS_EXPIRY_TIME = 5000;

  const LEVEL_ROWS = 15;
  const LEVEL_COLS = 11;

  const level = [];

  const COL_PADDING = 0;
  const ROW_PADDING = -1;

  const width = (TILE_WIDTH + COL_PADDING) * LEVEL_COLS + UI_WIDTH;
  const height = (TILE_HEIGHT + ROW_PADDING) * (LEVEL_ROWS + 1);

  canvas.style.width = `${width}px`;
  canvas.style.height = `${height}px`;
  canvas.style.overflow = 'hidden;'

  canvas.width = width;
  canvas.height = height;

  const player = {
    position: {
      x: 1,
      y: 1,
    },
    attributes: {
      hp: 3,
      speed: 1,
      bombs: 10,
      fires: 1,
      invincible: false,
    },
  };

  const nextLevelDoor = {
    position: { x: -1, y: -1 },
  };

  let bombs = {
    timers: new Set(),
    intervals: new Set(),
    explosionTimers: new Set(),
  };

  const bonuses = new Set();

  const enemies = new Set();

  const ENEMY_UPDATE_SPEED = {
    [ENEMY_LAMP]: 700,
    [ENEMY_BOAR]: 200,
    [ENEMY_BROOM]: 400,
  };

  const killPlayer = () => {
    if (player.attributes.invincible === true) {
      return;
    }

    player.attributes.hp--;

    if (player.attributes.hp > 0) {
      player.attributes.invincible = true;

      const t1 = setInterval(() => {
        renderTile(player.position.x, player.position.y, INVINCIBLE_PLAYER);
      }, INVINCIBLE_INTERVAL);

      setTimeout(() => {
        player.attributes.invincible = false;
        clearInterval(t1);
      }, INVINCIBLE_TIMEOUT);
    }
  };

  const updateLamp = (descriptor) => {
    // selects random location and goes there; respects all walls
    const { position: { x: px, y: py }, destination, path } = descriptor;

    if (destination && (px !== destination.x || py !== destination.y) && path.length) {
      const { x: nx, y: ny } = path.pop();

      if (player.position.x === px && player.position.y === py) {
        killPlayer();
      }

      if (level[nx][ny] === TILE_EMPTY) {
        descriptor.position = { x: nx, y: ny };

        render();

        return;
      }
    }

    const tmpLevel = [];

    for (let row of level) {
      tmpLevel.push(row.map(c => c !== TILE_EMPTY ? Number.MAX_VALUE : 0));
    }

    tmpLevel[px][py] = 1;

    const checkedPositions = [];
    const queue = [{ x: px, y: py }];
    let maxSteps = 0;

    while (queue.length > 0) {
      const { x, y } = queue.pop();

      checkedPositions.push({ x, y });

      maxSteps = Math.max(tmpLevel[x][y], maxSteps);

      if (x - 1 > -1 && tmpLevel[x - 1][y] === 0) {
        tmpLevel[x - 1][y] = tmpLevel[x][y] + 1;
        queue.unshift({ x: x - 1, y });
      }

      if (x + 1 < tmpLevel.length && tmpLevel[x + 1][y] === 0) {
        tmpLevel[x + 1][y] = tmpLevel[x][y] + 1;
        queue.unshift({ x: x + 1, y });
      }

      if (y - 1 > -1 && tmpLevel[x][y - 1] === 0) {
        tmpLevel[x][y - 1] = tmpLevel[x][y] + 1;
        queue.unshift({ x, y: y - 1 });
      }

      if (y + 1 < tmpLevel[0].length && tmpLevel[x][y + 1] === 0) {
        tmpLevel[x][y + 1] = tmpLevel[x][y] + 1;
        queue.unshift({ x, y: y + 1 });
      }
    }

    const { x: dx, y: dy } = checkedPositions.find(({ x: x1, y: y1 }) => tmpLevel[x1][y1] === maxSteps);

    const newPath = [{ x: dx, y: dy }];
    let currentPosition = {...newPath[newPath.length - 1]};

    while (tmpLevel[currentPosition.x][currentPosition.y] > 1) {
      const n = tmpLevel[currentPosition.x][currentPosition.y] - 1;

      if (currentPosition.x - 1 > -1 && tmpLevel[currentPosition.x - 1][currentPosition.y] === n) {
        newPath.push({ x: currentPosition.x - 1, y: currentPosition.y });
      } else if (currentPosition.x + 1 < tmpLevel.length && tmpLevel[currentPosition.x + 1][currentPosition.y] === n) {
        newPath.push({ x: currentPosition.x + 1, y: currentPosition.y });
      } else if (currentPosition.y - 1 > -1 && tmpLevel[currentPosition.x][currentPosition.y - 1] === n) {
        newPath.push({ x: currentPosition.x, y: currentPosition.y - 1 });
      } else if (currentPosition.y + 1 < tmpLevel[0].length && tmpLevel[currentPosition.x][currentPosition.y + 1] === n) {
        newPath.push({ x: currentPosition.x, y: currentPosition.y + 1 });
      } else {
        console.error('pathfinding error ¯\\_(ツ)_/¯')
        break;
      }

      currentPosition = {...newPath[newPath.length - 1]};
    }

    // newPath.shift();
    newPath.pop();

    descriptor.destination = { x: dx, y: dy };
    descriptor.path = newPath;
  };

  const updateBoar = (descriptor) => {
    // always follows player; respects all walls
  };

  const updateBroom = (descriptor) => {
    // picks random location and goes there; ignores brick walls
  };

  const ENEMY_AI_UPDATE = {
    [ENEMY_LAMP]: updateLamp,
    [ENEMY_BOAR]: updateBoar,
    [ENEMY_BROOM]: updateBroom,
  };

  const createEnemy = (x, y, type) => {
    const descriptor = { position: { x, y }, type };

    const t1 = setInterval(ENEMY_AI_UPDATE[type].bind(null, descriptor), ENEMY_UPDATE_SPEED[type]);

    descriptor.interval = t1;

    enemies.add(descriptor);
  };

  const createBonus = (x, y, type) => {
    const descriptor = { position: { x, y }, type, revealed: false };

    const reveal = () => {
      const t1 = setTimeout(() => {
        const t2 = setInterval(() => {
          const blinkType = {
            [BONUS_FIRE1]: BONUS_FIRE2,
            [BONUS_FIRE2]: BONUS_FIRE1,
            [BONUS_BOMB1]: BONUS_BOMB2,
            [BONUS_BOMB2]: BONUS_FIRE1,
            [BONUS_LIFE1]: BONUS_LIFE2,
            [BONUS_LIFE2]: BONUS_LIFE1,
          };

          renderTile(x, y, blinkType[type]);
        }, BONUS_UPDATE_INTERVAL);

        descriptor.interval = t2;
      }, BONUS_START_BLINK_TIME);

      const t3 = setTimeout(() => {
        destroyBonus(descriptor);
      }, BONUS_EXPIRY_TIME);

      descriptor.timer1 = t1;
      descriptor.timer2 = t3;
      descriptor.revealed = true;
    };

    descriptor.reveal = reveal;

    bonuses.add(descriptor);
  };

  const generateLevel = (rows, cols) => {
    player.position = { x: 1, y: 1 };

    bombs.timers.forEach(t => clearTimeout(t));
    bombs.intervals.forEach(t => clearInterval(t));
    bombs.explosionTimers.forEach(t => clearTimeout(t));

    bombs.timers.clear();
    bombs.intervals.clear();
    bombs.explosionTimers.clear();

    // clear level
    let emptySlots = 0;

    for (let i = 0; i < rows; ++i) {
      level[i] = [];

      for (let t = 0; t < cols; ++t) {
        if (i === 0 || t === 0 || i === rows - 1 || t === cols - 1 || ((i % 2) === 0 && (t % 2) === 0)) {
          level[i][t] = TILE_PERMANENT_WALL;
          continue;
        }

        level[i][t] = TILE_EMPTY;
        ++emptySlots;
      }
    }

    nextLevelDoor.position = { x: -1, y: -1 };

    const newWalls = Math.floor(emptySlots / 3) + (Math.floor(Math.random() * 100) % (emptySlots / 3));
    const newEnemies = 1 + (Math.floor(Math.random() * 10) % 5);

    let filled = 0;

    while (filled < newWalls) {
      const x = Math.floor(Math.random() * 100) % rows;
      const y = Math.floor(Math.random() * 100) % cols;

      // permanent walls must not be touched
      if (level[x][y] == TILE_PERMANENT_WALL) {
        continue;
      }

      // player starting positions must be clear
      if ((x == 1 && y == 1) || (x == 2 && y == 1) || (x == 1 && y == 2)) {
        continue;
      }

      // generate a new enemy
      if (enemies.size < newEnemies) {
        createEnemy(x, y, ENEMY_TYPE[Math.floor(Math.random() * 100) % ENEMY_TYPE.length]);
        continue;
      }

      // do not create walls where enemy is
      if (Array.from(enemies).find(({ position: { x: ex, y: ey } }) => x === ex && y === ey)) {
        continue;
      }

      // generate exit, if it does not exist yet
      if (nextLevelDoor.position.x === -1 || nextLevelDoor.position.y === -1) {
        nextLevelDoor.position = { x, y };
      }

      // optionally, generate a bonus, if there is no existing bonus in this place already
      if ((Math.floor(Math.random() * 100) % 2) === 0 && !Array.from(bonuses).find(({ position: { x: bx, y: by } }) => x === bx && y === by)) {
        createBonus(x, y, BONUS_TYPE[Math.floor(Math.random() * 100) % BONUS_TYPE.length]);
      }

      // put the brick wall
      level[x][y] = TILE_BRICK_WALL;
      ++filled;
    }
  };

  const destroyBonus = (descriptor) => {
    const { timer1, timer2, interval, type } = descriptor;

    clearTimeout(timer1);
    clearTimeout(timer2);
    clearInterval(interval);

    bonuses.delete(descriptor);
  };

  const pickUpBonus = (descriptor) => {
    destroyBonus(descriptor);

    const { type } = descriptor;

    if (type === BONUS_BOMB1 || type === BONUS_BOMB2) {
      player.attributes.bombs++;
    } else if (type === BONUS_FIRE1 || type === BONUS_FIRE2) {
      player.attributes.fires++;
    } else if (type === BONUS_LIFE1 || type === BONUS_LIFE2) {
      player.attributes.hp++;
    }
  };

  const movePlayer = (dx, dy) => {
    // check for collisions
    let { position: { x, y } } = player;

    x += dx;
    y += dy;

    if (Array.from(enemies).find(({ position: { x: ex, y: ey } }) => ex === x && ey === y)) {
      killPlayer();
    }

    const bonus = Array.from(bonuses).find(({ position: { x: bx, y: by } }) => bx === x && by === y);

    if (bonus) {
      pickUpBonus(bonus);
    }

    const cell = level[x][y];

    if (cell === TILE_NEXT_LEVEL_DOOR1 || cell === TILE_NEXT_LEVEL_DOOR2) {
      generateLevel(LEVEL_ROWS, LEVEL_COLS);
      render();
      return;
    }

    if ([ TILE_BRICK_WALL, TILE_PERMANENT_WALL, TILE_BOMB1, TILE_BOMB2 ].includes(cell)) {
      return;
    }

    player.position = { x, y };
  };

  const explodeBomb = (x, y) => {
    const EXPLOSION_STOPPER_TILES = [TILE_BRICK_WALL, TILE_BOMB1, TILE_BOMB2, TILE_EXPLOSION];

    const { fires } = player.attributes;

    const explosionFires = [{ x, y }];

    for (let i = x - 1; i > -1 && i > x - 1 - fires; --i) {
      if (level[i][y] === TILE_PERMANENT_WALL) {
        break;
      }

      explosionFires.push({ x: i, y });

      if (EXPLOSION_STOPPER_TILES.includes(level[i][y])) {
        break;
      }
    }

    for (let i = x + 1; i < level.length && i < x + 1 + fires; ++i) {
      if (level[i][y] === TILE_PERMANENT_WALL) {
        break;
      }

      explosionFires.push({ x: i, y });

      if (EXPLOSION_STOPPER_TILES.includes(level[i][y])) {
        break;
      }
    }

    for (let i = y - 1; i > -1 && i > y - 1 - fires; --i) {
      if (level[x][i] === TILE_PERMANENT_WALL) {
        break;
      }

      explosionFires.push({ x, y: i });

      if (EXPLOSION_STOPPER_TILES.includes(level[x][i])) {
        break;
      }
    }

    for (let i = y + 1; i < level[0].length && i < y + 1 + fires; ++i) {
      if (level[x][i] === TILE_PERMANENT_WALL) {
        break;
      }

      explosionFires.push({ x, y: i });

      if (EXPLOSION_STOPPER_TILES.includes(level[x][i])) {
        break;
      }
    }

    for (let { x: cx, y: cy } of explosionFires) {
      // kill enemy
      for (let enemyDescriptor of enemies) {
        const { position: { x: ex, y: ey }, interval } = enemyDescriptor;

        if (ex === cx && ey === cy) {
          clearInterval(enemyDescriptor.interval);
          enemies.delete(enemyDescriptor);
        }
      }

      // kill player
      if (player.position.x === cx && player.position.y === cy) {
        killPlayer();
      }

      // reveal or destroy bonus
      for (let bonusDescriptor of bonuses) {
        const { position: { x: bx, y: by }, reveal } = bonusDescriptor;

        if (bx === cx && by === cy) {
          if (level[cx][cy] === TILE_BRICK_WALL) {
            reveal.call(null);
          } else {
            destroyBonus(bonusDescriptor);
          }
        }
      }

      level[cx][cy] = TILE_EXPLOSION;

      if (level[cx][cy] === TILE_BOMB1 || level[cx][cy] === TILE_BOMB2) {
        explodeBomb(cx, cy);
      }
    }

    render();

    const t2 = setTimeout(() => {
      bombs.explosionTimers.delete(t2);

      // clear explosion fires
      for (let { x: cx, y: cy } of explosionFires) {
        if (cx === nextLevelDoor.position.x && cy === nextLevelDoor.position.y) {
          level[cx][cy] = TILE_NEXT_LEVEL_DOOR0;

          const t1 = setInterval(() => {
            if (enemies.size > 0) {
              return;
            }

            const state = level[nextLevelDoor.position.x][nextLevelDoor.position.y];

            if (state === TILE_NEXT_LEVEL_DOOR1) {
              level[nextLevelDoor.position.x][nextLevelDoor.position.y] = TILE_NEXT_LEVEL_DOOR2;
            } else {
              level[nextLevelDoor.position.x][nextLevelDoor.position.y] = TILE_NEXT_LEVEL_DOOR1;
            }
          }, NEXT_LEVEL_DOOR_INTERVAL);
        } else {
          level[cx][cy] = TILE_EMPTY;
        }
      }

      render();
    }, EXPLOSION_DURATION);

    bombs.explosionTimers.add(t2);
  };

  const placeBomb = () => {
    const { position: { x, y }, attributes: { bombs: maxBombs } } = player;

    if (bombs.timers.size >= maxBombs) {
      return;
    }

    level[x][y] = TILE_BOMB1;

    let state = TILE_BOMB1;

    const t1 = setInterval(() => {
      if (state === TILE_BOMB1) {
        state = TILE_BOMB2;
      } else {
        state = TILE_BOMB1;
      }

      level[x][y] = state;

      render();
    }, BOMB_TICK_INTERVAL);

    const t2 = setTimeout(() => {
      explodeBomb(x, y);
      clearInterval(t1);
      bombs.intervals.delete(t1);
      bombs.timers.delete(t2);
    }, BOMB_TICK_TIME);

    bombs.intervals.add(t1);
    bombs.timers.add(t2);
  };

  const onKeyDown = (e) => {
    const keyCodes = {
      [KEY_MOVE_RIGHT]: () => movePlayer(0, -1),
      [KEY_MOVE_LEFT]: () => movePlayer(1, 0),
      [KEY_MOVE_UP]: () => movePlayer(-1, 0),
      [KEY_MOVE_DOWN]: () => movePlayer(0, 1),
      [KEY_PLACE_BOMB]: () => placeBomb(),
    };

    const fn = keyCodes[e.keyCode];

    if (fn) {
      fn.call(null);
      render();
    }
  };

  const destroyCanvas = () => {
    canvas.parentElement.removeChild(canvas);
  };

  const onClick = () => {
    if (player.attributes.hp <= 0) {
      uninitInput();
      destroyCanvas();
    }
  };

  const initInput = () => {
    document.addEventListener('keydown', onKeyDown);
    canvas.addEventListener('click', onClick);
  };

  const uninitInput = () => {
    document.removeEventListener('keydown', onKeyDown);
  };

  const renderUI = () => {
    ctx.font = `${TILE_HEIGHT}px courier new`;
    ctx.fillStyle = '#000';

    const strings = [
      `${TILES[BONUS_BOMB1][0]} x ${player.attributes.bombs}`,
      `${TILES[BONUS_FIRE1][0]} x ${player.attributes.fires}`,
      `${TILES[BONUS_LIFE1][0]} x ${player.attributes.hp}`,
      `-----------`,
      `h/l - left/right`,
      `j/k - down/up`,
      `b - place bomb`,
    ];

    for (let i = 0; i < strings.length; ++i) {
      ctx.fillText(strings[i], (level[0].length + 3) * TILE_WIDTH, TILE_HEIGHT * (2 + i));
    }
  };

  const renderTile = (x, y, tile) => {
    const [ char, style ] = TILES[tile];

    ctx.clearRect(y * (TILE_WIDTH + COL_PADDING), x * (TILE_HEIGHT + ROW_PADDING), (TILE_WIDTH + COL_PADDING), (TILE_HEIGHT + ROW_PADDING));

    ctx.font = `${TILE_HEIGHT}px courier new`;
    ctx.fillStyle = style;
    ctx.fillText(char, y * (TILE_WIDTH + COL_PADDING), (x + 1) * (TILE_HEIGHT + ROW_PADDING));
  };

  const renderEndOfGame = () => {
    const fontSize = TILE_HEIGHT * 2;

    ctx.fillStyle = 'black';
    ctx.font = `${fontSize}px courier new`;

    const strings = [
      `GAME OVER`,
      `---------`,
      `click to close`,
    ];

    for (let i = 0; i < strings.length; ++i) {
      const text = strings[i];
      const textWidth = ctx.measureText(text).width;

      ctx.fillText(text, (width / 2) - (textWidth / 2), (height / 2) + ((fontSize / 2) * i));
    }
  };

  const render = () => {
    ctx.clearRect(0, 0, width, height);

    if (player.attributes.hp < 1) {
      renderEndOfGame();
      return;
    }

    for (let i = 0; i < level.length; ++i) {
      for (let t = 0; t < level[i].length; ++t) {
        renderTile(i, t, level[i][t]);
      }
    }

    renderTile(player.position.x, player.position.y, TILE_PLAYER);

    for (const { position: { x, y }, type } of enemies) {
      renderTile(x, y, type);
    }

    for (const { position: { x, y }, type, revealed } of bonuses) {
      if (revealed) {
        renderTile(x, y, type);
      }
    }

    renderUI();
  };

  generateLevel(LEVEL_ROWS, LEVEL_COLS);
  initInput();
  render();
};

window.__startRogueBomber = __startRogueBomber;

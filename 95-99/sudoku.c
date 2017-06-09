#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define SUDOKU_N 3
#define BLOCK_N (SUDOKU_N * SUDOKU_N)
#define DIGITS_N BLOCK_N
#define PLACE_BIT_N (BLOCK_N * BLOCK_N)
#define BLOCK_BIT_N (BLOCK_N * DIGITS_N)
#define ROW_BIT_N  BLOCK_BIT_N
#define COL_BIT_N ROW_BIT_N
#define SET_COVER_ROW (BLOCK_N * BLOCK_N * DIGITS_N)
#define SET_COVER_COL (PLACE_BIT_N + BLOCK_BIT_N + ROW_BIT_N + COL_BIT_N)

typedef struct DNode {
    struct DNode *left, *right, *up, *down;
    struct DNode *ctl;
    uint32_t size, row;
} DNode; /* the node of dlx */

int bitmap[SET_COVER_ROW][SET_COVER_COL];
int row_seq[SET_COVER_ROW];

int puzzle[BLOCK_N][BLOCK_N];
int backref[SET_COVER_ROW][3];

DNode *dnode_new() {
    return (DNode *)malloc(sizeof(DNode));
}

void dnode_free(DNode *p) {
    free(p);
}

DNode *build_dlx(int nrow, int ncol) {
    DNode *head = dnode_new();
    DNode *chead[SET_COVER_COL];
    head->left = head->right = head;
    head->up = head->down = head;
    int i, j;
    /* build the guard chain */
    for (j = 0; j < ncol; j++)
    {
        DNode *p = chead[j] = dnode_new();
        (p->left = head->left)->right = p;
        (p->right = head)->left = p;
        p->up = p->down = p;
    }
    DNode *rhead = dnode_new(); /* helper node */
    for (i = 0; i < nrow; i++)
    {
        rhead->left = rhead->right = rhead;
        for (j = 0; j < ncol; j++)
            if (bitmap[i][j])
            {
                DNode *p = dnode_new();
                p->ctl = chead[j];
                p->row = i;
                (p->left = rhead->left)->right = p;
                (p->right = rhead)->left = p;
                (p->up = p->ctl->up)->down = p;
                (p->down = p->ctl)->up = p;
            }
        rhead->left->right = rhead->right;
        rhead->right->left = rhead->left;
    }
    dnode_free(rhead);
    return head;
}

#define LOOP(q, p, d) for (q = p->d; q != p; q = q->d)

void set_cover(DNode *pctl) {
    DNode *q, *r;
    pctl->left->right = pctl->right;
    pctl->right->left = pctl->left;
    LOOP(q, pctl, down)
        LOOP(r, q, right)
        {
            r->up->down = r->down;
            r->down->up = r->up;
            r->ctl->size--;
        }
}

void set_uncover(DNode *pctl) {
    DNode *q, *r;
    pctl->left->right = pctl;
    pctl->right->left = pctl;
    LOOP(q, pctl, up)
        LOOP(r, q, left)
        {
            r->up->down = r;
            r->down->up = r;
            r->ctl->size++;
        }
}

void dlx_print(int step) {
    int i;
    for (i = 0; i < step; i++)
        printf("%d ", row_seq[i]);
    puts("");
}

void sudoku_print(int step) {
    int i, j;
    for (i = 0; i < step; i++)
    {
        int *t = backref[row_seq[i]];
        puzzle[t[0]][t[1]] = t[2];
    }
    for (i = 0; i < BLOCK_N; i++, puts(""))
        for (j = 0; j < BLOCK_N; j++)
            printf("%d ", puzzle[i][j]);
    puts("");
}

void search(DNode *head, int step) {
    DNode *p, *q, *lp;
    uint32_t least_value = -1U;
    if (head->left == head)
    {
        sudoku_print(step);
        /* dlx_print(step); */
        return;
    }
    LOOP(p, head, right)
        if (p->size < least_value)
        {
            least_value = p->size;
            lp = p;
        }

    set_cover(lp);
    LOOP(p, lp, down)
    {
        LOOP(q, p, right) set_cover(q->ctl);
        row_seq[step] = q->row;
        search(head, step + 1);
        LOOP(q, p, left) set_uncover(q->ctl);
    }
    set_uncover(lp);
}

void test_dlx() {
    int nrow, ncol, i, j;
    DNode *head;
    scanf("%d %d", &nrow, &ncol);
    for (i = 0; i < nrow; i++)
        for (j = 0; j < ncol; j++)
            scanf("%d", bitmap[i] + j);
    head = build_dlx(nrow, ncol);
    search(head, 0);
}

void sudoku() {
    int i, j, d, nrow = 0;
    DNode *head;
    for (i = 0; i < BLOCK_N; i++)
        for (j = 0; j < BLOCK_N; j++)
            scanf("%d", puzzle[i] + j);
    for (i = 0; i < BLOCK_N; i++)
        for (j = 0; j < BLOCK_N; j++)
            for (d = 0; d < DIGITS_N; d++)
                if (!puzzle[i][j] || puzzle[i][j] == d + 1)
                {
                    int *b = bitmap[nrow];
                    b[i * BLOCK_N + j] = 1;
                    b += PLACE_BIT_N;
                    b[((i / SUDOKU_N) * SUDOKU_N + j / SUDOKU_N) * BLOCK_N + d] = 1;
                    b += BLOCK_BIT_N;
                    b[i * BLOCK_N + d] = 1;
                    b += ROW_BIT_N;
                    b[j * BLOCK_N + d] = 1;
                    backref[nrow][0] = i;
                    backref[nrow][1] = j;
                    backref[nrow][2] = d + 1;
                    nrow++;
                }
    head = build_dlx(nrow, SET_COVER_COL);
    search(head, 0);
}

int main() {
    /* test_dlx(); */
    sudoku();
    return 0;
}

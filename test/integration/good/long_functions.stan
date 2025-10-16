functions {
  array[] int vote_count(array[] int rating, array[] int item,
                         array[] int rater, int I, int J) {
    int N = size(rating);
    array[I] int count_by_item = rep_array(1, I); // index 0:5 by 1:6
    for (n in 1 : N) {
      count_by_item[item[n]] += rating[n];
    }
    array[J + 1] int count = rep_array(0, J + 1);
    for (i in 1 : I) {
      count[count_by_item[i]] += 1;
    }
    return count;
  }
  
  array[] int vote_countvote_countvote_countvote_count(array[] int rating,
                                                       array[] int item,
                                                       array[] int rater,
                                                       int I, int J) {
    int N = size(rating);
    array[I] int count_by_item = rep_array(1, I); // index 0:5 by 1:6
    for (n in 1 : N) {
      count_by_item[item[n]] += rating[n];
    }
    array[J + 1] int count = rep_array(0, J + 1);
    for (i in 1 : I) {
      count[count_by_item[i]] += 1;
    }
    return count;
  }
  
  tuple(array[] int, array[] int, real) vote_countvote_countvote_countvote_count2(
  array[] int rating, array[] int item, array[] int rater, int I, int J) {
    int N = size(rating);
    array[I] int count_by_item = rep_array(1, I); // index 0:5 by 1:6
    for (n in 1 : N) {
      count_by_item[item[n]] += rating[n];
    }
    array[J + 1] int count = rep_array(0, J + 1);
    for (i in 1 : I) {
      count[count_by_item[i]] += 1;
    }
    return (count, item, 3.14);
  }
}

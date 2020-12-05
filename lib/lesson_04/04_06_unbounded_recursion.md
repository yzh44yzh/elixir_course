## Unbounded recursion

Unbounded recursion is when we can’t predict the number of repetitions for a recursive function. For example, it’s hard to predict how many iterations a web crawler that navigates and downloads web pages will have. 

 similar problem occurs when software tries to map a machine’s file system, even if it’s a more controlled environment than the web. Each directory it finds can have many more directories inside. Let’s explore this type of recursion, creating a program that prints and navigates through a given system directory. 
 Годится как упражнение

To create more predictable functions, let’s write some code to reduce the number of iterations. We’ll explore two strategies—one that focuses on limiting the number of iterations and another that focuses on avoiding infinite loops.

We’ll add a depth restriction that will flag how many child directories deep we want to dive from the given directory. 
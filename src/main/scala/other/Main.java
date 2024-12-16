package other;
// We have imported the necessary tool classes.
// If you need to import additional packages or classes, please import here.
import java.math.BigInteger;
import java.util.HashSet;
import java.util.Scanner;
import java.util.Set;

public class Main {

    public static int cnt=-1;

    public static Set<Integer> set=new HashSet<>();

    public static void main(String[] args) {
        // please define the JAVA input here. For example: Scanner s = new Scanner(System.in);
        // please finish the function body here.
        // please define the JAVA output here. For example: System.out.println(s.nextInt())
        Scanner in=new Scanner(System.in);
        String[] m=in.nextLine().split(","),n=in.nextLine().split(","),hei=in.nextLine().split(" "),wid=in.nextLine().split(" ");
        int m1=Integer.valueOf(m[0]),m2=Integer.valueOf(m[1]);
        int n1=Integer.valueOf(n[0]),n2=Integer.valueOf(n[1]);
        int[][] h=new int[m1][m2];
        int[][] o=new int[m1][m2];
        for (int i=0;i<m1;i++){
            String[] temp=hei[i].split(",");
            for (int j=0;j<m2;j++){
                h[i][j]=Integer.valueOf(temp[j]);
            }
        }
        for (int i=0;i<m1;i++){
            String[] temp=wid[i].split(",");
            for (int j=0;j<m2;j++){
                o[i][j]=Integer.valueOf(temp[j]);
            }
        }
        set.add(n1*100+n2);
        BigInteger cur=BigInteger.ONE.shiftLeft(n1).or(BigInteger.ONE.shiftLeft(n2));
        getSum(h,o,1,cur.or(BigInteger.ONE.shiftLeft(n1-1).and(BigInteger.ONE.shiftLeft(n2))),n1-1,n2,n1,n2);
        getSum(h,o,1,cur.or(BigInteger.ONE.shiftLeft(n1).and(BigInteger.ONE.shiftLeft(n2-1))),n1,n2-1,n1,n2);
        getSum(h,o,1,cur.or(BigInteger.ONE.shiftLeft(n1+1).and(BigInteger.ONE.shiftLeft(n2))),n1+1,n2,n1,n2);
        getSum(h,o,1,cur.or(BigInteger.ONE.shiftLeft(n1).and(BigInteger.ONE.shiftLeft(n2+1))),n1,n2+1,n1,n2);
        System.out.println(cnt);
        cur.intValue();
    }

    private static void getSum(int[][] h, int[][] o, int speed, BigInteger route, int n1, int n2,int p1,int p2) {
        if (set.contains(n1*100+n2)) return;
        if (n1<0||n1>=h.length||n2<0||n2>=h[0].length) return;
        speed+=h[p1][p2]-h[n1][n2]-o[n1][n2];
        if (speed<0) return;
        if (speed==1&&!set.contains(n1*100+n2)) {
            set.add(n1*100+n2);
            cnt++;
        }
        BigInteger route1=BigInteger.ONE.shiftLeft(n1-1).and(BigInteger.ONE.shiftLeft(n2));
        BigInteger route2=BigInteger.ONE.shiftLeft(n1).and(BigInteger.ONE.shiftLeft(n2-1));
        BigInteger route3=BigInteger.ONE.shiftLeft(n1+1).and(BigInteger.ONE.shiftLeft(n2));
        BigInteger route4=BigInteger.ONE.shiftLeft(n1).and(BigInteger.ONE.shiftLeft(n2+1));
        if (route.and(route1).bitCount()!=2) getSum(h,o,1,route1.or(route),n1-1,n2,n1,n2);
        if (route.and(route2).bitCount()!=2) getSum(h,o,1,route2.or(route),n1,n2-1,n1,n2);
        if (route.and(route3).bitCount()!=2) getSum(h,o,1,route3.or(route),n1+1,n2,n1,n2);
        if (route.and(route4).bitCount()!=2) getSum(h,o,1,route4.or(route),n1,n2+1,n1,n2);
    }
}

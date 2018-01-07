int i = 0;

void setup(){
    background(0);
    size(300, 300);
}

void draw(){
    stroke(random(255), random(0), random(0), 100);
    line(i, 0, random(0, width), height);
    if(i < width){
        i = 1 + i;
    }
    else{
        i = 0;
    }
}
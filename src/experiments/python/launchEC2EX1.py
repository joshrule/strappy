#!/usr/bin/env python

import sys
from glob import glob
import subprocess 
import boto
from boto.exception import EC2ResponseError
import time

EC2_KEYPAIR_NAME = 'ec2-sample-key'
IMAGE_ID = 'ami-e57fea8c' 

def launch_spot_instances(count=1):
    ec2 = boto.connect_ec2()
    spot_instance_info = {
        'price' : .04,
        'key_name' : EC2_KEYPAIR_NAME,
        'image_id' : IMAGE_ID,
        'count' : count,
        'type' : "one-time",
        'security_groups' : ['quick-start-1'],
        'instance_type' : 'm1.large'}
        
    requests = ec2.request_spot_instances(**spot_instance_info)
    return requests

def test_ssh_connection(inst):
    script = "ssh -o StrictHostKeyChecking=no -o BatchMode=yes -i ~/.ssh/ec2-sample-key.pem ubuntu@%s \"exit;\" " \
        % inst.public_dns_name
    print script
    out = subprocess.Popen(script, shell=True, 
                     stderr=subprocess.PIPE, 
                     stdout=subprocess.PIPE)
    print "Testing ssh connection on inst %s." % inst.id
    returncode = out.wait()
    (outstr, err) = out.communicate()
    print "Returncode: %s" % returncode
    if returncode == 0:
        return True
    else:
        print "SSH error: %s" % err
        return False

def send_ex1_job(inst):
    script = """
cd ~/
rm -fr program-induction
git clone git@bitbucket.org:edechter/program-induction.git
cd ~/program-induction/src/PI/
ghc --make -O2 -rtsopts  -main-is Experiments.EX1.main -o runEX1 Experiments/EX1.hs 
./runEX1 1000 5000 10000
git pull
git add data/EX1*
git commit -m \'Data from ec2 instance %s running ex1 experiment (TEST)\'
git push
""" % (inst.id)
    
    dns = inst.public_dns_name
    sshcmd = "ssh -o StrictHostKeyChecking=no -i ~/.ssh/ec2-sample-key.pem ubuntu@%s \"%s\"; exit;" % (dns, script)
    print sshcmd
    print "Waiting a minute to make sure ssh server is ready..."
    while True:
        success = test_ssh_connection(inst)
        if success:
            break
    print "Okay, ready to continue."
    logfilename = "./ec2.ex1.%s.log" % inst.id
    fid = open(logfilename, 'a')
    out = subprocess.Popen(sshcmd, shell=True, stderr=fid, stdout=fid)
    return out 


def get_instance_id_if_spot_is_fulfilled(request):
    ec2 = boto.connect_ec2()
    req = ec2.get_all_spot_instance_requests([request.id])[0]
    if req.status.code=='fulfilled':
        return req.instance_id
    else:
        return None

def get_instances_by_id(instance_ids):
    ec2 = boto.connect_ec2()
    if instance_ids == []:
        return []
    else:
        reservations = ec2.get_all_instances(instance_ids=instance_ids)
        instances = [r.instances[0] for r in reservations]
        return instances

def instances_all_have_dns_names(instances):
    for inst in instances:
        if len(inst.public_dns_name) == 0:
            return False
    return True


def send_ex1_jobs_when_ready(requests):
    ec2 = boto.connect_ec2()
    remaining_requests = requests
    while True:
        if len(remaining_requests) == 0:
            break
        time.sleep(5)
        reqs_and_instance_ids = [(r, get_instance_id_if_spot_is_fulfilled(r))\
                            for r in requests]
        print reqs_and_instance_ids
        instance_ids = [i for (r, i) in reqs_and_instance_ids if not i is None]
        print "Instance ids that are fulfilled: ", instance_ids
        if len(instance_ids)==0:
            continue
        remaining_requests = [r for (r, i) in reqs_and_instance_ids if i is None]
        while True:
            try:
                instances = get_instances_by_id(instance_ids)
                if instances_all_have_dns_names(instances):
                    print "All instances have dns names."
                    break
                print "Not all instances have dns names... waiting..."
                time.sleep(5)
            except EC2ResponseError, err:
                print "Got EC2ResponseError. ", err
                print "Trying again."
                time.sleep(5)

        for inst in instances:
            print "" 
            print "EX1 job sent to instance %s at dns %s" % (inst.id, inst.public_dns_name)
            send_ex1_job(inst)
            time.sleep(10)

        print "Remaining requests: ", remaining_requests
    print "Jobs sents to all requests."


def launch_spots_send_jobs(count=1):
    requests = launch_spot_instances(count)
    send_ex1_jobs_when_ready(requests)
    
    
if __name__ == "__main__":
    launch_spots_send_jobs(count=50)

    



